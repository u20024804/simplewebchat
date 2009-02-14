-module(webchat).

-export([start/0, stop/0]).

start() ->
    application:start(mnesia),
    application:start(webchat).
    
stop() ->
    application:stop(webchat),
    application:stop(mnesia).
