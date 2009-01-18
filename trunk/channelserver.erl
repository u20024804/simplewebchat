-module(channelserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([join/2, leave/2, userlist/1]).

-import(lists, [umerge/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, ets:new(man, [private, bag])}.

handle_cast({join, Who, Channel}, Men) ->
    ets:insert(Men, {Channel, Who}),
    {noreply, Men};
    
handle_cast({leave, Who, Channel}, Men) ->
    ets:delete_object(Men, {Channel, Who}),
    {noreply, Men}.
    
handle_call({userlist, Channel}, _From, Men) ->
    {reply, ets:lookup(Men, Channel), Men}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _Men) ->
    ok.
    
join(Who, Channel) ->
    gen_server:cast(?MODULE, {join, Who, Channel}).
    
leave(Who, Channel) ->
    gen_server:cast(?MODULE, {leave, Who, Channel}).
    
userlist(Channel) ->
    gen_server:call(?MODULE, {userlist, Channel}).

