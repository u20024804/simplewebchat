-module(config).

-export([url/0]).


url() ->
    [
        {urldispatch, "/", fun processer:index/2}
    ].
