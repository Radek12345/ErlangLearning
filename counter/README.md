# counter

An OTP application

## Build

rebar3 compile
rebar3 shell

--- mnesia helper -----

rm -rf Mnesia* _.DCD _.LOG

mnesia:stop().
mnesia:delete_schema([node()]).
mnesia:create_schema([node()]).

rebar3 shell --name radek@localhost --setcookie mycookie
