-module(msgserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([dispatch/2, popmsg/2]).

-define(TTL, 15000).
-define(DELAY, 10).
-define(GEN_CAST, '$gen_cast').

-import(lists, [foreach/2]).

-import(whereserver, [where/2, whois/1]).
-import(channelserver, [userlist/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, ets:new(message, [private, duplicate_bag])}.
    
handle_cast({push, Who, Channel, Msg}, Msgs) ->
    case where(Who, Channel) of
        {address, notfound} ->
            ets:insert(Msgs, {{Who, Channel}, Msg, make_ref()}),
            {noreply, Msgs};
        {address, Channel, Who, From} ->
            Tag = make_ref(),
            From ! {self(), Tag, {message, Channel, Msg}},
            receive
                {Tag, finish} ->
                    {noreply, Msgs}
            after ?DELAY ->
                ets:insert(Msgs, {{Who, Channel}, Msg, make_ref()}),
                {noreply, Msgs}
            end
    end;
    
handle_cast({pop, Who, Channel, From}, Msgs) ->
    case ets:lookup(Msgs, {Who, Channel}) of
        [] ->
            void;
        [H|_] ->
            {{Who, Channel}, Msg, _Tag} = H,
            Tag = make_ref(),
            From ! {self(), Tag, {message, Channel, Msg}},
            receive
                {Tag, finish} ->
                    ets:delete_object(Msgs, H)
            after ?DELAY ->
                void
            end
    end,
    {noreply, Msgs}.
        
handle_call(_Request, _From, State) ->
    {reply, void, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
dispatch(Msg, Channel) ->
    foreach(fun({_Channel, Who}) ->
            gen_server:cast(?MODULE, {push, Who, Channel, Msg})
        end, userlist(Channel)).
    
popmsg(Who, Channel) ->
    gen_server:cast(?MODULE, {pop, Who, Channel, self()}),
    receive
        {Server, Tag, {message, Channel, Msg}} ->
            Server ! {Tag, finish},
            Msg
    after ?TTL ->
        ""
    end.

