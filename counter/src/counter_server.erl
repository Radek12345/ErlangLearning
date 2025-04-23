-module(counter_server).

-behaviour(gen_server).

-export([start_link/0, increment/0, decrement/0, get/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

decrement() ->
    gen_server:cast(?MODULE, decrement).

get() ->
    gen_server:call(?MODULE, get).

init([]) ->
    {ok, load_counter()}.

handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(increment, State) ->
    NewState = State + 1,
    ok = save_counter(NewState),
    {noreply, NewState};
handle_cast(decrement, State) ->
    NewState = State - 1,
    ok = save_counter(NewState),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_counter() ->
    case mnesia:dirty_read(counter, counter_id) of
        [{counter, counter_id, Value}] ->
            Value;
        [] ->
            ok = mnesia:dirty_write({counter, counter_id, 0}),
            0
    end.

save_counter(Value) ->
    mnesia:dirty_write({counter, counter_id, Value}).
