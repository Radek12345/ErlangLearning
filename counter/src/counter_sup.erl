-module(counter_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 10},
    ChildSpecs =
        [#{id => counter_server,
           start => {counter_server, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [counter_server]}],
    {ok, {SupFlags, ChildSpecs}}.
