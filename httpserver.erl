-module(httpserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([acceptor_loop/2]).

-include("head.hrl").
-import(gen_tcp, [listen/2, accept/1, close/1, send/2, recv/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1,
                packet_cookie/1, write_packet/1, read_packet/1, format/2]).


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
            #request{action={Method, Location, _, _}} = PhrReq,
            Dispatcher = case [Dispatcher || {urldispatch, Location0, Dispatcher}
                    <- config:url(), lists:prefix(Location0, Location)] of
                    [] ->
                        fun processer:notfound/2;
                    [FirstDispatcher|_] ->
                        FirstDispatcher
                end,
            PhrReqWithCookie = #request{setcookie=SetCookie0} = check_cookie(PhrReq),
            Response = #response{heads=Heads1, setcookie=SetCookie1} =
                try Dispatcher(Method, PhrReqWithCookie)
                catch
                    throw:Throw ->
                        #response{body=format("throw: ~p~n", [Throw])};
                    exit:Exit ->
                        #response{body=format("exit: ~p~n", [Exit])};
                    error:Error ->
                        #response{body=format("error: ~p~n", [Error])}
                end,
            SetCookie = case SetCookie0 of
                    [] ->
                        SetCookie1;
                    [SessionId] ->
                        [SessionId | SetCookie1]
                end,
            Heads = case SetCookie of
                    [] ->
                        Heads1;
                    _ ->
                        [{head, "Set-Cookie", packet_cookie(SetCookie)} | Heads1]
                end,
            RawResponse = packet_response(Response#response{heads=Heads}),
            send(Socket, write_packet(RawResponse));
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
    
check_cookie(Request=#request{heads=Heads}) ->
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
    Request#request{sessionid=SessionId, cookies=Cookies, setcookie=SetCookie,
            sessionbegin=SessionBegin}.

