-module(config).

-export([url/0]).


url() ->
    [
        {urldispatcher, "/message", fun processer:index/2},
        {urldispatcher, "/register", fun processer:register/2},
        {urldispatcher, "/login", fun processer:login/2}
    ].
