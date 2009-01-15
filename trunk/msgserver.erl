-module(msgserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([dispatch/1, popmsg/0]).

-define(TTL, 15000).
-define(GEN_CAST, '$gen_cast').

-import(lists, [foreach/2]).

-import(whereserver, [whois/1]).
-import(channelserver, [userlist/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, []}.
    
handle_cast({push, Who, Msg}, Msgs) ->
    {noreply, Msgs ++ [{message, Who, Msg}]}.
    
handle_call({pop}, {From, _Tag}, Msgs) ->
    {address, Who, From} = whois(From),
    case [M || M = {message, W, _} <- Msgs, W =:= Who] of
        [] ->
            receive
                {?GEN_CAST, {push, Who, Msg}} ->
                    {reply, Msg, Msgs}
                after ?TTL ->
                    {reply, "", Msgs}
            end;
        [Msg|_] ->
            {reply, Msg, lists:delete(Msg, Msgs)}
    end.
    
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
    gen_server:call(?MODULE, {pop}, infinity).   

