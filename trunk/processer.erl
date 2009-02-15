-module(processer).

-export([index/2, register/2, notfound/2, login/2, join/2]).

-include("head.hrl").
-import(lists, [concat/1]).
-import(whereserver, [iam/1, off/1]).
-import(msgserver, [popmsg/1, dispatch/3]).
-import(sessionserver, [get_session/2, del_session/2, set_session/3]).
-import(phrase, [phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).
-import(util, [input/2, do/1]).

index(get, #request{sessionid=SessionId}) ->
    {session, found, User} = get_session(SessionId, "username"),
    iam(User),
    Msg = filter(format("~p", [popmsg(User)])),
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

index(post, #request{action=Action, data=Data, sessionid=SessionId}) ->
    {session, found, User} = get_session(SessionId, "username"),
    Inputs = querystr(Data),
    Outs = [format("input: #~s#, #~s#<br/>\n", [Key, Value]) || {input, Key, Value} <- Inputs],
    {_Method, "/message/" ++ Uri, _Version, _UrlQuery} = Action,
    Channel = case Uri of
            "group/" ++ Group ->
                {channel, group, Group};
            "friend/" ++ Friend ->
                {channel, friend, Friend};
            _ ->
                {channel, error}
        end,
    dispatch(User, Channel, concat(Outs)),
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
    {atomic, Users} = mnesia:transaction(fun() -> mnesia:read({user, Username}) end),
    case [User || User = #user{password=Password0} <- Users, Password0 =:= Password] of
            [] ->
                Body = "login fail";
            [_] ->
                set_session(SessionId, "username", Username),
                Body = "login success"
        end,
    #response{body=Body}.
    
join(post, #request{data=Data}) ->
    Inputs = querystr(Data),
    User = input(Inputs, "user"),
    Channel = input(Inputs, "channel"),
    channelserver:join(User, Channel),
    #response{body=format("~p joined ~p~n", [User, Channel])}.

notfound(Method, Request) when Method =:= get; Method =:= post ->
    {_, Location, _, _} = Request#request.action,
    Body = format("the page requested not found: \"~s\"", [Location]),
    #response{body=Body}.

