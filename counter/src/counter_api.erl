-module(counter_api).

-export([init/2, start_link/0]).

start_link() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/counters", ?MODULE, []},
                                 {"/counters/:id", ?MODULE, []},
                                 {"/counters/:id/increment", ?MODULE, []},
                                 {"/counters/:id/decrement", ?MODULE, []},
                                 {"/counters/:id/reset", ?MODULE, []}]}]),
    {ok, _} =
        cowboy:start_clear(counter_api, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    {ok, self()}.

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    error_logger:info_msg("Method: ~p, Path: ~p~n", [Method, Path]),
    case {Method, Path} of
        {<<"GET">>, <<"/counters">>} ->
            Counters = counter_server:list_counters(),
            error_logger:info_msg("GET /counters, counters: ~p~n", [Counters]),
            Req = cowboy_req:reply(200,
                                   #{<<"content-type">> => <<"application/json">>},
                                   jiffy:encode(#{counters =>
                                                      [atom_to_binary(C, utf8) || C <- Counters]}),
                                   Req0),
            {ok, Req, State};
        {<<"GET">>, _} ->
            IdBin = cowboy_req:binding(id, Req0),
            case is_valid_id(IdBin) of
                true ->
                    CounterId = binary_to_atom(IdBin, utf8),
                    Value = counter_server:get(CounterId),
                    error_logger:info_msg("GET /counters/~p, value: ~p~n", [CounterId, Value]),
                    Req = cowboy_req:reply(200,
                                           #{<<"content-type">> => <<"application/json">>},
                                           jiffy:encode(#{value => Value}),
                                           Req0),
                    {ok, Req, State};
                false ->
                    error_logger:info_msg("Invalid counter ID: ~p~n", [IdBin]),
                    reply_error(400, <<"Invalid counter ID">>, Req0, State)
            end;
        {<<"POST">>, Path} ->
            IdBin = cowboy_req:binding(id, Req0),
            case is_valid_id(IdBin) of
                true ->
                    CounterId = binary_to_atom(IdBin, utf8),
                    ExpectedIncrementPath = <<"/counters/", IdBin/binary, "/increment">>,
                    ExpectedDecrementPath = <<"/counters/", IdBin/binary, "/decrement">>,
                    ExpectedResetPath = <<"/counters/", IdBin/binary, "/reset">>,
                    case Path of
                        ExpectedIncrementPath ->
                            error_logger:info_msg("POST /counters/~p/increment~n", [CounterId]),
                            counter_server:increment(CounterId),
                            reply_ok(Req0, State);
                        ExpectedDecrementPath ->
                            error_logger:info_msg("POST /counters/~p/decrement~n", [CounterId]),
                            counter_server:decrement(CounterId),
                            reply_ok(Req0, State);
                        ExpectedResetPath ->
                            error_logger:info_msg("POST /counters/~p/reset received~n",
                                                  [CounterId]),
                            {ok, Body, Req1} = cowboy_req:read_body(Req0),
                            error_logger:info_msg("Body: ~p~n", [Body]),
                            try jiffy:decode(Body, [return_maps]) of
                                #{<<"value">> := NewValue} when is_integer(NewValue) ->
                                    error_logger:info_msg("Calling counter_server:reset(~p, ~p)~n",
                                                          [CounterId, NewValue]),
                                    counter_server:reset(CounterId, NewValue),
                                    reply_ok(Req1, State);
                                _ ->
                                    error_logger:info_msg("Invalid JSON format~n"),
                                    reply_error(400, <<"Invalid or missing value">>, Req1, State)
                            catch
                                error:badarg ->
                                    error_logger:info_msg("JSON decode failed: badarg~n"),
                                    reply_error(400, <<"Invalid JSON">>, Req1, State)
                            end;
                        _ ->
                            error_logger:info_msg("Unknown POST path: ~p~n", [Path]),
                            reply_error(404, <<"Not found">>, Req0, State)
                    end;
                false ->
                    error_logger:info_msg("Invalid counter ID: ~p~n", [IdBin]),
                    reply_error(400, <<"Invalid counter ID">>, Req0, State)
            end;
        _ ->
            error_logger:info_msg("Unknown request: Method=~p, Path=~p~n", [Method, Path]),
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

is_valid_id(Id) ->
    is_binary(Id) andalso byte_size(Id) > 0 andalso byte_size(Id) =< 32.
