-module(counter_server).

-behaviour(gen_server).

-export([start_link/0, increment/1, decrement/1, get/1, list_counters/0, delete/1,
         get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         reset/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment(Id) ->
    gen_server:cast(?MODULE, {increment, Id}).

decrement(Id) ->
    gen_server:cast(?MODULE, {decrement, Id}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

reset(Id, NewValue) ->
    gen_server:cast(?MODULE, {reset, Id, NewValue}).

delete(Id) ->
    gen_server:cast(?MODULE, {delete, Id}).

list_counters() ->
    gen_server:call(?MODULE, list_counters).

get_history(Id) ->
    gen_server:call(?MODULE, {get_history, Id}).

init([]) ->
    {ok, #{}}.

handle_call({get, Id}, _From, State) ->
    case load_counter(Id) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        {expired, _OldVal} ->
            {reply, {error, expired}, State}
    end;
handle_call(list_counters, _From, State) ->
    Keys = mnesia:dirty_all_keys(counter),
    {reply, Keys, State};
handle_call({get_history, Id}, _From, State) ->
    Records = mnesia:dirty_read(counter_history, Id),
    JsonReady =
        lists:map(fun({counter_history, _, Op, Ts}) ->
                     #{operation => atom_to_binary(Op, utf8),
                       timestamp => list_to_binary(datetime_to_rfc3339(Ts))}
                  end,
                  Records),
    {reply, JsonReady, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({increment, Id}, State) ->
    case load_counter(Id) of
        {ok, Value} ->
            NewValue = Value + 1,
            ok = save_counter(Id, NewValue),
            log_operation(Id, increment);
        {expired, _} ->
            ok
    end,
    {noreply, State};
handle_cast({decrement, Id}, State) ->
    case load_counter(Id) of
        {ok, Value} ->
            NewValue = Value - 1,
            ok = save_counter(Id, NewValue),
            log_operation(Id, decrement);
        {expired, _} ->
            ok
    end,
    {noreply, State};
handle_cast({reset, Id, NewValue}, State) ->
    ok = save_counter(Id, NewValue),
    log_operation(Id, reset),
    {noreply, State};
handle_cast({delete, Id}, State) ->
    mnesia:dirty_delete({counter, Id}),
    log_operation(Id, delete),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("UNHANDLED cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_counter(Id) ->
    case mnesia:dirty_read(counter, Id) of
        [{counter, Id, Value, ExpiresAt}] ->
            case expired(ExpiresAt) of
                true ->
                    {expired, 0};
                false ->
                    {ok, Value}
            end;
        [] ->
            ok = save_counter(Id, 0),
            {ok, 0}
    end.

save_counter(Id, Value) ->
    mnesia:dirty_write({counter, Id, Value, ttl()}).

log_operation(Id, Operation) ->
    Timestamp = calendar:universal_time(),
    mnesia:dirty_write({counter_history, Id, Operation, Timestamp}).

datetime_to_rfc3339({Date, Time}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = {Date, Time},
    io_lib:format("~4..0B-~2..0B-~2..0B" ++ "T" ++ "~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec]).

ttl() ->
    erlang:system_time(second) + 500.  % 5 minut od teraz

expired(undefined) ->
    false;
expired(ExpiresAt) ->
    Now = erlang:system_time(second),
    Now > ExpiresAt.
