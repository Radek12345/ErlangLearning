-module(counter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case init_mnesia() of
        ok ->
            counter_sup:start_link();
        Error ->
            {error, Error}
    end.

stop(_State) ->
    ok.

init_mnesia() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(counter, [{attributes, [id, value]}, {disc_copies, [node()]}]),
    mnesia:wait_for_tables([counter], 10000).
