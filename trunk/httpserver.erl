-module(httpserver).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([acceptor_loop/2]).

-import(gen_tcp, [listen/2, accept/1, close/1, send/2, recv/2]).
-import(lists, [concat/1]).

-import(channelserver, [join/2]).
-import(whereserver, [iam/2, off/2]).
-import(msgserver, [popmsg/2, dispatch/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).


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
            {Status, Heads, Body} = process(PhrReq),
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
    

process({request, Action, Heads, Data}) ->
    case [C ||{head, "Cookie", C} <- Heads] of
        [] ->
            SessionId = create_sessionId(),
            Cookies = [{cookie, "SessionId", SessionId}];
        [C] ->
            case [SessionId || {cookie, "SessionId", SessionId} <- phrase_cookie(C)] of
                [] ->
                    SessionId = create_sessionId(),
                    Cookies = [{cookie, "SessionId", SessionId}];
                [SessionId] ->
                    Cookies = []
            end
    end,
        
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {Method, Channel, _Version, _UrlQuery} = Action,
    Msg = if
        Method =:= post ->
            dispatch(concat(Outs), Channel),
            "";
        length(Cookies) =/= 0 ->
            join(SessionId, Channel),
            "";
        true ->
            join(SessionId, Channel),
            iam(SessionId, Channel),
            M = filter(popmsg(SessionId, Channel)),            
            off(SessionId, Channel),
            M
    end,
    Body = format("function response(){return 'get: ~s'}", [Msg]),
    Status = "200 OK",
    Heads2 = [{head, "Content-Type", "text/html; charset=utf-8"},
            {head, "Cache-Control", "no-cache"}],
    case Cookies of
        [] ->
            Heads3 = Heads2;
        _ ->
            Heads3 = [{head, "Set-Cookie", packet_cookie(Cookies)} | Heads2]
    end,

    {Status, Heads3, Body}.

