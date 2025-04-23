# counter

An OTP application

## Build

rebar3 compile
rebar3 shell --name radek@localhost --setcookie mycookie

--- mnesia helper -----

rm -rf Mnesia\* _.DCD _.LOG

mnesia:stop().
mnesia:delete_schema([node()]).
mnesia:create_schema([node()]). ----------------- Wydaje się że to wystarczy odpalić w konsoli i restart i śmiga
mnesia:start().
mnesia:create_table(counter, [
{attributes, [id, value]},
{disc_copies, [node()]},
{type, set}
]).

rebar3 shell --name radek@localhost --setcookie mycookie
