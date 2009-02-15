-module(config).

-export([url/0]).


url() ->
    [
        {url, "/message", fun processer:index/2},
        {url, "/register", fun processer:register/2},
        {url, "/login", fun processer:login/2},
        {url, "/join", fun processer:join/2}
    ].
