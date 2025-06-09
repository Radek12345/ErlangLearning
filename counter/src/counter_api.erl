-module(counter_api).

-export([init/2, start_link/0]).

start_link() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/counters", ?MODULE, []},
                                 {"/counters/:id", ?MODULE, []},
                                 {"/counters/:id/increment", ?MODULE, []},
                                 {"/counters/:id/decrement", ?MODULE, []},
                                 {"/counters/:id/reset", ?MODULE, []},
                                 {"/counters/:id/history", ?MODULE, []}]}]),
    {ok, _} =
        cowboy:start_clear(counter_api, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    {ok, self()}.

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"GET">>, <<"/counters">>} ->
            Counters = counter_server:list_counters(),
            Req = cowboy_req:reply(200,
                                   #{<<"content-type">> => <<"application/json">>},
                                   jiffy:encode(#{counters =>
                                                      lists:map(fun(C) -> atom_to_binary(C, utf8)
                                                                end,
                                                                Counters)}),
                                   Req0),
            {ok, Req, State};
        {<<"GET">>, _} ->
            IdBin = cowboy_req:binding(id, Req0),
            CounterId = binary_to_atom(IdBin, utf8),
            HistoryPath = <<"/counters/", IdBin/binary, "/history">>,
            case Path of
                HistoryPath ->
                    History = counter_server:get_history(CounterId),
                    Req = cowboy_req:reply(200,
                                           #{<<"content-type">> => <<"application/json">>},
                                           jiffy:encode(#{history => History}),
                                           Req0),
                    {ok, Req, State};
                _ ->
                    case counter_server:get(CounterId) of
                        {ok, Value} ->
                            RespJson = jiffy:encode(#{value => Value});
                        {error, expired} ->
                            RespJson = jiffy:encode(#{error => <<"expired">>})
                    end,
                    Req = cowboy_req:reply(200,
                                           #{<<"content-type">> => <<"application/json">>},
                                           RespJson,
                                           Req0),
                    {ok, Req, State}
            end;
        {<<"POST">>, Path} ->
            IdBin = cowboy_req:binding(id, Req0),
            CounterId = binary_to_atom(IdBin, utf8),
            IncrementPath = <<"/counters/", IdBin/binary, "/increment">>,
            DecrementPath = <<"/counters/", IdBin/binary, "/decrement">>,
            ResetPath = <<"/counters/", IdBin/binary, "/reset">>,
            case Path of
                IncrementPath ->
                    counter_server:increment(CounterId),
                    reply_ok(Req0, State);
                DecrementPath ->
                    counter_server:decrement(CounterId),
                    reply_ok(Req0, State);
                ResetPath ->
                    {ok, Body, Req1} = cowboy_req:read_body(Req0),
                    #{<<"value">> := NewValue} = jiffy:decode(Body, [return_maps]),
                    counter_server:reset(CounterId, NewValue),
                    reply_ok(Req1, State);
                _ ->
                    reply_error(404, <<"Not found">>, Req0, State)
            end;
        {<<"DELETE">>, _} ->
            IdBin = cowboy_req:binding(id, Req0),
            CounterId = binary_to_atom(IdBin, utf8),
            counter_server:delete(CounterId),
            reply_ok(Req0, State);
        _ ->
            reply_error(404, <<"Not found">>, Req0, State)
    end.

reply_ok(Req, State) ->
    Req2 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{status => <<"ok">>}),
                         Req),
    {ok, Req2, State}.

reply_error(Code, Msg, Req, State) ->
    Req2 =
        cowboy_req:reply(Code,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{error => Msg}),
                         Req),
    {ok, Req2, State}.
