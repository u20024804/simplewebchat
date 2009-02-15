-module(util).

-export([input/2, do/1]).

input(Inputs, Name) ->
    [Input] = [Input || {input, Name0, Input} <- Inputs, Name =:= Name0],
    Input.
    
do(Q) ->
    {atomic, L} = mnesia:transaction(fun() -> qlc:e(Q) end),
    L.
