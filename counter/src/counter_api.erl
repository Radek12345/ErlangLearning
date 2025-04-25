-module(counter_api).

-export([start_link/0, init/2]).

start_link() ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/counter", ?MODULE, []},
                                 {"/counter/increment", ?MODULE, []},
                                 {"/counter/decrement", ?MODULE, []},
                                 {"/counter/reset", ?MODULE, []}]}]),
    {ok, _} =
        cowboy:start_clear(counter_api, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    {ok, self()}.

init(Req = #{method := <<"GET">>, path := <<"/counter">>}, State) ->
    Value = counter_server:get(),
    Req2 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{value => Value}),
                         Req),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>, path := <<"/counter/increment">>}, State) ->
    counter_server:increment(),
    Req2 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{status => <<"ok">>}),
                         Req),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>, path := <<"/counter/decrement">>}, State) ->
    counter_server:decrement(),
    Req2 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{status => <<"ok">>}),
                         Req),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>, path := <<"/counter/reset">>}, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case jiffy:decode(Body, [return_maps]) of
        #{<<"value">> := NewValue} when is_integer(NewValue) ->
            counter_server:reset(NewValue),
            Req2 =
                cowboy_req:reply(200,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jiffy:encode(#{status => <<"ok">>}),
                                 Req1),
            {ok, Req2, State};
        _ ->
            Req2 =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"application/json">>},
                                 jiffy:encode(#{error => <<"Invalid or missing value">>}),
                                 Req1),
            {ok, Req2, State}
    end;
init(Req, State) ->
    Req2 =
        cowboy_req:reply(404,
                         #{<<"content-type">> => <<"application/json">>},
                         jiffy:encode(#{error => <<"Not found">>}),
                         Req),
    {ok, Req2, State}.
