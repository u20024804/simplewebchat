-module(webchat).

-export([start/0, stop/0]).

start() ->
    application:start(webchat).
    
stop() ->
    application:stop(webchat).
