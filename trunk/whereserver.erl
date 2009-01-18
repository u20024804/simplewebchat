-module(whereserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([iam/2, off/2, where/2, whois/1]).

-import(lists, [umerge/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, ets:new(address, [private])}.
    
handle_cast(_, State) ->
    {noreply, State}.
    
handle_call({iam, Who, Channel}, {From, _Tag}, Addrs) ->
    ets:insert(Addrs, {{Who, Channel}, From}),
    {reply, void, Addrs};
    
handle_call({off, Who, Channel}, {_From, _Tag}, Addrs) ->
    ets:delete(Addrs, {Who, Channel}),
    {reply, void, Addrs};
    
handle_call({where, Who, Channel}, {_From, _Tag}, Addrs) ->
    Ws = ets:lookup(Addrs, {Who, Channel}),
    W = case Ws of
            [] ->
                {address, notfound};
            [Addr|_] ->
                {{Who, Channel}, From} = Addr,
                {address, Channel, Who, From}
        end,
    {reply, W, Addrs};
    
handle_call({whois, From}, {_From, _Tag}, Addrs) ->
    Ws = ets:select(Addrs, [{{'$1','$2'},[{'=:=','$2',{const,From}}],['$_']}]),
    W = case Ws of
            [] ->
                {address, notfound};
            [Addr|_] ->
                {{Who, Channel}, From} = Addr,
                {address, Channel, Who, From}
        end,
    {reply, W, Addrs}.

handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Addrs) ->
    ok.

    
iam(Who, Channel) ->
    gen_server:call(?MODULE, {iam, Who, Channel}).
    
off(Who, Channel) ->
    gen_server:call(?MODULE, {off, Who, Channel}).
    
where(Who, Channel) ->
    gen_server:call(?MODULE, {where, Who, Channel}).
    
whois(From) ->
    gen_server:call(?MODULE, {whois, From}).


