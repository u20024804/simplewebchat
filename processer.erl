-module(processer).

-export([index/2, register/2]).


-include("head.hrl").
-import(lists, [concat/1]).
-import(channelserver, [join/2]).
-import(whereserver, [iam/2, off/2]).
-import(msgserver, [popmsg/2, dispatch/2]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).

index(get, #request{action=Action, sessionid=SessionId, sessionbegin=false}) ->
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
    #response{heads=Heads, body=Body};
    
index(get, #request{action=Action, sessionid=SessionId, sessionbegin=true}) ->
    {_Method, Channel, _Version, _UrlQuery} = Action,
    join(SessionId, Channel),
    Body = "function response(){return ''}",
    #response{body=Body};

index(post, #request{action=Action, data=Data}) ->
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {_Method, Channel, _Version, _UrlQuery} = Action,
    dispatch(concat(Outs), Channel),
    #response{}.
    
register(post, #request{data=Data}) ->
    Inputs = querystr(Data),
    [Username] = [Username || {input, "username", Username} <- Inputs],
    [Password] = [Password || {input, "password", Password} <- Inputs],
    User = #user{username=Username, password=Password},
    mnesia:write(User),
    Body = "register success",
    #response{body=Body}.

