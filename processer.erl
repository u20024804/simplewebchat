-module(processer).

-export([index/2, register/2, notfound/2, login/2]).

-include("head.hrl").
-include_lib("stdlib/include/qlc.hrl").
-import(lists, [concat/1]).
-import(channelserver, [join/2]).
-import(whereserver, [iam/1, off/1]).
-import(msgserver, [popmsg/2, dispatch/2]).
-import(sessionserver, [get_session/2, del_session/2, set_session/3]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).
-import(util, [input/2, do/1]).

index(get, #request{action=Action, sessionid=SessionId}) ->
    {_Method, Channel, _Version, _UrlQuery} = Action,
    {session, found, User} = get_session(SessionId, "username"),
    iam(User),
    Msg = filter(popmsg(User, Channel)),            
    off(User),
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

index(post, #request{action=Action, data=Data}) ->
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {_Method, Channel, _Version, _UrlQuery} = Action,
    dispatch(concat(Outs), Channel),
    #response{}.
    
register(post, #request{data=Data}) ->
    Inputs = querystr(Data),
    Username = input(Inputs, "username"),
    Password = input(Inputs, "password"),
    User = #user{username=Username, password=Password},
    mnesia:transaction(fun() -> mnesia:write(User) end),
    Body = "register success",
    #response{body=Body}.
    
login(post, #request{data=Data, sessionid=SessionId}) ->
    Inputs = querystr(Data),
    Username = input(Inputs, "username"),
    Password = input(Inputs, "password"),
    case do(qlc:q([User || User = {user, Username0, Password0} <-
        mnesia:table(user), Username0 =:= Username, Password0 =:= Password])) of
            [] ->
                Body = "login fail";
            [_] ->
                set_session(SessionId, "username", Username),
                Body = "login success"
        end,
    #response{body=Body}.

notfound(Method, Request) when Method =:= get; Method =:= post ->
    {_, Location, _, _} = Request#request.action,
    Body = format("the page requested not found: \"~s\"", [Location]),
    #response{body=Body}.

