-module(processer).

-export([index/2]).


-import(lists, [concat/1]).
-import(channelserver, [join/2]).
-import(whereserver, [iam/2, off/2]).
-import(msgserver, [popmsg/2, dispatch/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).

index(Method, {request, Action, _Heads, Data, Cookies, SessionId, SessionBegin}) ->
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {Method, Channel, _Version, _UrlQuery} = Action,
    Msg = if
        Method =:= post ->
            dispatch(concat(Outs), Channel),
            "";
        SessionBegin =:= true ->
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

