# counter

An OTP application

## Build

rebar3 compile
rebar3 shell

## Run

curl http://localhost:8080/counter
curl -X POST http://localhost:8080/counter/increment
curl -X POST http://localhost:8080/counter/decrement
curl -X POST -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/counter/reset



--- mnesia helper -----

rm -rf Mnesia* _.DCD _.LOG

mnesia:stop().
mnesia:delete_schema([node()]).
mnesia:create_schema([node()]).

rebar3 shell --name radek@localhost --setcookie mycookie
