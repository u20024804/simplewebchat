-module(msgserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([dispatch/1, popmsg/0]).

-define(TTL, 15000).
-define(DELAY, 10).
-define(GEN_CAST, '$gen_cast').

-import(lists, [foreach/2]).

-import(whereserver, [where/1]).
-import(channelserver, [userlist/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, []}.
    
handle_cast({push, Who, Msg}, Msgs) ->
    case where(Who) of
        {address, notfound} ->
            {noreply, Msgs ++ [{message, Who, Msg}]};
        {address, Who, From} ->
            Tag = make_ref(),
            From ! {self(), Tag, {message, Msg}},
            receive
                {Tag, finish} ->
                    {noreply, Msgs}
            after ?DELAY ->
                {noreply, Msgs ++ [{message, Who, Msg}]}
            end
    end.
        
handle_call(_Request, _From, State) ->
    {reply, void, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
dispatch(Msg) ->
    foreach(fun(Who) ->
            gen_server:cast(?MODULE, {push, Who, Msg})
        end, userlist()).
    
popmsg() ->
    receive
        {Server, Tag, {message, Msg}} ->
            Server ! {Tag, finish},
            Msg
    after ?TTL ->
        ""
    end.

