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
    Node = node(),
    SchemaDir = mnesia:system_info(directory),

    %% Tworzymy schema tylko jeśli katalog Mnesia nie istnieje
    SchemaResult =
        case filelib:is_dir(SchemaDir) of
            true ->
                io:format("==> Schema already exists~n"),
                ok;
            false ->
                io:format("==> Creating schema~n"),
                %% najpierw zatrzymaj, jeśli działa
                case mnesia:system_info(is_running) of
                    yes ->
                        mnesia:stop();
                    no ->
                        ok
                end,
                case mnesia:create_schema([Node]) of
                    ok ->
                        io:format("Schema created~n"),
                        ok;
                    {error, {_, {already_exists, _}}} ->
                        io:format("Schema already exists (race condition?)~n"),
                        ok;
                    Error ->
                        io:format("Error creating schema: ~p~n", [Error]),
                        {error, Error}
                end
        end,

    case SchemaResult of
        ok ->
            ok;
        Error3 ->
            {error, Error3}
    end,

    io:format("==> Starting Mnesia~n"),
    case mnesia:start() of
        ok ->
            io:format("Mnesia start OK~n");
        {error, Reason} ->
            io:format("Error starting Mnesia: ~p~n", [Reason]),
            {error, Reason}
    end,

    case mnesia:system_info(is_running) of
        yes ->
            io:format("Mnesia is running~n"),
            ok;
        no ->
            io:format("Mnesia is NOT running!~n"),
            {error, mnesia_not_running}
    end,

    io:format("==> Creating table~n"),
    case mnesia:create_table(counter,
                             [{attributes, [id, value]}, {disc_copies, [Node]}, {type, set}])
    of
        {atomic, ok} ->
            io:format("Table created successfully~n"),
            ok;
        {aborted, {already_exists, counter}} ->
            io:format("Table already exists~n"),
            ok;
        {aborted, Reason2} ->
            io:format("Error creating table: ~p~n", [Reason2]),
            {error, Reason2}
    end,

    io:format("==> Waiting for tables~n"),
    case mnesia:wait_for_tables([counter], 10000) of
        ok ->
            io:format("Table is ready~n"),
            ok;
        Error2 ->
            io:format("Error waiting for table: ~p~n", [Error2]),
            {error, Error2}
    end.
