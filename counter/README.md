# counter

An OTP application

## Build

rebar3 compile
rebar3 shell

## Run

curl http://localhost:8080/counters
curl http://localhost:8080/counters/:id
curl http://localhost:8080/counters/:id/history
curl -X POST http://localhost:8080/counters/:id/increment
curl -X POST http://localhost:8080/counters/:id/decrement
curl -X POST -H "Content-Type: application/json" -d '{"value": 42}' http://localhost:8080/counters/:id/reset
curl -X DELETE http://localhost:8080/counters/:id

--- mnesia helper -----

--- WITHOUT SIGN \ -----
rm -rf Mnesia\* _.DCD _.LOG

mnesia:stop().
mnesia:delete_schema([node()]).
mnesia:create_schema([node()]).

rebar3 shell --name radek@localhost --setcookie mycookie
