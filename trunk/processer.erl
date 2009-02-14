-module(processer).

-export([index/2]).


-import(lists, [concat/1]).
-import(channelserver, [join/2]).
-import(whereserver, [iam/2, off/2]).
-import(msgserver, [popmsg/2, dispatch/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).

index(get, {request, Action, _Heads, _Data, _Cookies, SetCookie, SessionId, false}) ->
    {_Method, Channel, _Version, _UrlQuery} = Action,
    join(SessionId, Channel),
    iam(SessionId, Channel),
    Msg = filter(popmsg(SessionId, Channel)),            
    off(SessionId, Channel),
    case Msg of
        [] ->
            Body = "function response(){return ''}",
            Heads = [];
        _ ->
            Body = format("function response(){return 'get: ~s'}", [Msg]),
            Heads = [{head, "Content-Type", "text/html; charset=utf-8"},
                    {head, "Cache-Control", "no-cache"}]
    end,
    Status = "200 OK",
    {Status, Heads, SetCookie, Body};
    
index(get, {request, Action, _Heads, _Data, _Cookies, SetCookie, SessionId, true}) ->
    {_Method, Channel, _Version, _UrlQuery} = Action,
    join(SessionId, Channel),
    Body = "function response(){return ''}",
    Status = "200 OK",
    Heads = [],
    {Status, Heads, SetCookie, Body};

index(post, {request, Action, _Heads, Data, _Cookies, SetCookie, _SessionId, _}) ->
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {_Method, Channel, _Version, _UrlQuery} = Action,
    dispatch(concat(Outs), Channel),
    Body = "",
    Status = "200 OK",
    Heads = [],
    {Status, Heads, SetCookie, Body}.


