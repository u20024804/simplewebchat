-module(httpserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([acceptor_loop/2]).

-import(gen_tcp, [listen/2, accept/1, close/1, send/2, recv/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1,
                packet_cookie/1, write_packet/1, read_packet/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    case listen(8000, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]) of
        {ok, Listen} ->
            {ok, new_acceptor(Listen)}; 
        {error, Reason} ->
            {stop, Reason}
    end.
                             
new_acceptor(Listen) ->
    proc_lib:spawn_link(?MODULE, acceptor_loop, [self(), Listen]),
    Listen.
    
acceptor_loop(Server, Listen) ->
    case catch gen_tcp:accept(Listen) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted}),
            work(Socket);
        {error, closed} ->
            exit({error, closed});
        _Other ->
            exit({error, accept_failed})
    end.

work(Socket) ->
    case recv(Socket, 0) of
        {ok, Bin} ->
            Request = read_packet(Bin),
            PhrReq = phrase_request(Request),
            {_, {Method, Location, _, _}, _, _} = PhrReq,
            [Dispatcher|_] = [Dispatcher || {urldispatch, Location0, Dispatcher} <- config:url(), lists:prefix(Location0, Location)],
            PhrReqWithCookie = check_cookie(PhrReq),
            {Status, Heads0, SetCookie, Body} = Dispatcher(Method, PhrReqWithCookie),
            case SetCookie of
                [] ->
                    Heads = Heads0;
                _ ->
                    Heads = [{head, "Set-Cookie", packet_cookie(SetCookie)} | Heads0]
            end,
            Response = packet_response({Status, Heads, Body}),
            send(Socket, write_packet(Response));
        {error, closed} ->
            void
    end,
    close(Socket).
    
handle_cast({accepted}, Listen) ->
    {noreply, new_acceptor(Listen)}.
    
handle_call(_, _, State) ->
    {noreply, error, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, Listen) ->
    gen_tcp:close(Listen),
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
create_sessionId() ->
    binary_to_list(term_to_binary(make_ref())).
%    {_, _, A1} = now(),
%    {_, _, A2} = now(),
%    {_, _, A3} = now(),
%    random:seed(A1, A2, A3),
%    R = integer_to_list(random:uniform(100)),
%    io:format("gen Sid: ~p~n", [R]),
%    R.
    
check_cookie({request, Action, Heads, Data}) ->
    case [C ||{head, "Cookie", C} <- Heads] of
        [] ->
            SessionId = create_sessionId(),
            Cookies = [],
            SetCookie = [{cookie, "SessionId", SessionId}],
            SessionBegin = true;
        [C] ->
            Cookies = phrase_cookie(C),
            case [SessionId || {cookie, "SessionId", SessionId} <- Cookies] of
                [] ->
                    SessionId = create_sessionId(),
                    SetCookie = [{cookie, "SessionId", SessionId}],
                    SessionBegin = true;
                [SessionId] ->
                    SetCookie = [],
                    SessionBegin = false
            end
    end,
    {request, Action, Heads, Data, Cookies, SetCookie, SessionId, SessionBegin}.


