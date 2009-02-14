-module(config).

-export([url/0]).


url() ->
    [
        {urldispatch, "/message", fun processer:index/2}
    ].
