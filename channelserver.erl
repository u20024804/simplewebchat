-module(channelserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([join/2, leave/2, userlist/1]).

-include("head.hrl").
-include_lib("stdlib/include/qlc.hrl").
-import(util, [do/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, group}.

handle_cast({join, Who, Channel}, Groups) ->
    Group = #group{groupname=Channel, username=Who},
    mnesia:transaction(fun() -> mnesia:write(Group) end),
    {noreply, Groups};
    
handle_cast({leave, Who, Channel}, Groups) ->
    Group = #group{groupname=Channel, username=Who},
    mnesia:transaction(fun() -> mnesia:delete_object(Group) end),
    {noreply, Groups}.
    
handle_call({userlist, Channel}, _From, Groups) ->
    Members = do(qlc:q([Group#group.username || Group <- mnesia:table(Groups), 
        Group#group.groupname =:= Channel])),
    {reply, Members, Groups}.
    
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

