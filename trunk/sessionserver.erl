-module(sessionserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_session/3, get_session/2, del_session/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, ets:new(session, [private])}.
    
handle_cast({set, Session, Key, Value}, Sessions) ->
    ets:insert(Sessions, {{Session, Key}, Value}),
    {noreply, Sessions};
    
handle_cast({delete, Session, Key}, Sessions) ->
    ets:delete(Sessions, {Session, Key}),
    {noreply, Sessions}.
    
handle_call({get, Session, Key}, _From, Sessions) ->
    case ets:lookup(Sessions, {Session, Key}) of
        [] ->
            {reply, {session, notfound}, Sessions};
        [{{Session, Key}, Value}] ->
            {reply, {session, found, Value}, Sessions}
    end.

handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Addrs) ->
    ok.
    
    
set_session(Session, Key, Value) ->
    gen_server:cast(?MODULE, {set, Session, Key, Value}).
    
del_session(Session, Key) ->
    gen_server:cast(?MODULE, {delete, Session, Key}).
    
get_session(Session, Key) ->
    gen_server:call(?MODULE, {get, Session, Key}).

