-module(whereserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([iam/1, off/1, where/1, whois/1]).

-import(lists, [umerge/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_) ->
    {ok, []}.
    
handle_cast(_, State) ->
    {noreply, State}.
    
handle_call({iam, Who}, {From, _Tag}, Addrs) ->
    {reply, void, umerge(Addrs, [{address, Who, From}])};
    
handle_call({off, Who}, {From, _Tag}, Addrs) ->
    {reply, void, lists:delete({address, Who, From}, Addrs)};
    
handle_call({Q, WhoOrWhere}, _From, Addrs) when (Q =:= where) or (Q =:= whois) ->
    Ws = case Q of
            where ->
                [Addr || Addr = {address, Who, _} <- Addrs, Who =:= WhoOrWhere];
            whois ->
                [Addr || Addr = {address, _, Where} <- Addrs, Where =:= WhoOrWhere]
        end,
    W = case Ws of
            [] ->
                {address, notfound};
            [Addr|_] ->
                Addr
        end,
    {reply, W, Addrs}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Addrs) ->
    ok.

    
iam(Who) ->
    gen_server:call(?MODULE, {iam, Who}).
    
off(Who) ->
    gen_server:call(?MODULE, {off, Who}).
    
where(Who) ->
    gen_server:call(?MODULE, {where, Who}).

whois(From) ->
    gen_server:call(?MODULE, {whois, From}).

