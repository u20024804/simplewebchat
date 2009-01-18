-module(channelserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([join/1, leave/1, userlist/0]).

-import(lists, [umerge/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, ets:new(man, [private])}.

handle_cast({join, Who}, Men) ->
    ets:insert(Men, {Who}),
    {noreply, Men};
    
handle_cast({leave, Who}, Men) ->
    ets:delete(Men, Who),
    {noreply, Men}.
    
handle_call({userlist}, _From, Men) ->
    All = ets:select(Men, [{{'$1'},[],['$1']}]),
    {reply, All, Men}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _Men) ->
    ok.
    
join(Who) ->
    gen_server:cast(?MODULE, {join, Who}).
    
leave(Who) ->
    gen_server:cast(?MODULE, {leave, Who}).
    
userlist() ->
    gen_server:call(?MODULE, {userlist}).
