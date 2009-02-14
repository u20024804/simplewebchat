-module(setup).

-export([database/0, tables/0]).


-include("head.hrl").


database() ->
    mnesia:create_schema([node()]).
    
tables() ->
    mnesia:start(),
    mnesia:create_table(user, [{disc_copies, [node()]},
            {attributes, record_info(fields, user)}]),
    mnesia:create_table(group, [{disc_copies, [node()]},
            {attributes, record_info(fields, group)}]),
    mnesia:create_table(message, [{disc_copies, [node()]},
            {attributes, record_info(fields, message)}]),
    mnesia:stop().
