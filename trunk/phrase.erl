-module(phrase).

-export([phrase_request/1, phrase_cookie/1, packet_response/1, packet_cookie/1,
        write_packet/1, read_packet/1, querystr/1, format/2, decode/1, filter/1]).
  
-include("head.hrl").
-import(erlang, [list_to_integer/2]).
-import(lists, [sublist/2, split/2, map/2, append/1, reverse/1, splitwith/2]).
-import(string, [str/2, chr/2, strip/1, tokens/2]).

phrase_request(Request) ->
    Boder = str(Request, "\r\n\r\n"),
    {Head, BodyWithLn} = split(Boder, Request),
    [$\n, $\r, $\n|Body] = BodyWithLn,
    [RawAction|RawHeads] = tokens(Head, "\r\n"),
    [Method, Url, Version] = tokens(RawAction, " "),
    {Location, QueryStr0} = splitwith(fun(C) -> C =/= $? end, Url),
    UrlQuery = case QueryStr0 of
        [] ->
            [];
        [$?|QueryStr1] ->
            try querystr(QueryStr1)
            catch
                error:{badmatch, _} -> {tag, QueryStr1}
            end
    end,
    Action = case Method of
        "POST" ->
            {post, Location, Version, UrlQuery};
        "GET" ->
            {get, Location, Version, UrlQuery}
    end,
    Heads = map(fun(H) ->
            {K0, V0} = split(chr(H, $:), H),
            K1 = sublist(K0, length(K0) - 1),
            {head, strip(K1), strip(V0)}
        end, RawHeads),
    #request{action=Action, heads=Heads, data=Body}.
    
phrase_cookie(Cookies) ->
    map(fun (C) ->
            {C0, V0} = split(chr(C, $=), C),
            C1 = sublist(C0, length(C0) - 1),
            {cookie, strip(C1), base64:decode_to_string(strip(V0))}
        end, tokens(Cookies, ";")).

packet_response(#response{status=Status, heads=Heads, body=Body}) ->
    Heads2 = [format("~s: ~s\r\n", [H, V]) || {head, H, V} <- Heads],
    Head = append(Heads2),
    format("HTTP/1.1 ~s\r\n~sContent-Length: ~p\r\n\r\n~s",
            [Status, Head, length(Body), Body]).
            
packet_cookie(Cookies) ->
    Cs = [format("~s=~s;", [K, base64:encode_to_string(V)]) || {cookie, K, V} <- Cookies],
    R = append(Cs),
    sublist(R, length(R) - 1).

write_packet(Str) ->
    list_to_binary(Str).

read_packet(Bin) ->
    binary_to_list(Bin).
            
format(Format, Str) ->
    L = io_lib:format(Format, Str),
    super_append(L).
        
super_append(L) ->
    super_append(L, []).
    
super_append([], R) ->
    lists:reverse(R);
    
super_append([H|T], R) when not is_list(H) ->
    super_append(T, [H|R]);
    
super_append([H|T], R) ->
    R1 = super_append(H, R),
    R2 = lists:reverse(R1),
    super_append(T, R2).
    
querystr(Str) ->
    Items = tokens(Str, "&"),
    map(fun(Item) ->
            [Key, RawValue] = tokens(Item, "="),
            Value = decode(RawValue),
            {input, Key, Value}
        end, Items).
        
decode(Str) ->
    reverse(decode(Str, [])).

decode([], Ret) ->
    Ret;

decode([H|T], Ret) when H =:= $% ->
    [B1, B2|T1] = T,
    C = list_to_integer([B1, B2], 16),
    decode(T1, [C|Ret]);
decode([H|T], Ret) when H =:= $+ ->
    decode(T, [$ |Ret]);

decode([H|T], Ret) ->
    decode(T, [H|Ret]).
    
filter(Str) ->
    filter(Str, []).
    
filter([], R) ->
    reverse(R);
    
filter([H|T], R) ->
    case H of
        $\\ ->
            filter(T, [$\\, $\\|R]);
        39 ->
            filter(T, [39, $\\|R]);
        34 ->
            filter(T, [34, $\\|R]);
        $\n ->
            filter(T, [$n, $\\|R]);
        $\r ->
            filter(T, [$r, $\\|R]);
        Any ->
            filter(T, [Any|R])
    end.

