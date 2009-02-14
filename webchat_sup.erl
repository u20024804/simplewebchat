-module(webchat_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

-define(MTTL, 60000).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 3, 1}, [
            {msgserver, {msgserver, start_link, []},
                permanent, brutal_kill, worker, [msgserver]},
            {channelserver, {channelserver, start_link, []},
                permanent, brutal_kill, worker, [channelserver]},
            {whereserver, {whereserver, start_link, []},
                permanent, brutal_kill, worker, [whereserver]},
            {httpserver, {httpserver, start_link, []},
                permanent, ?MTTL, worker, [httpserver]}
        ]}}.

