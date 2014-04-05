all: deps compile

compile:
  rebar compile
 
deps:
  rebar get-deps
 
generate:
  rebar generate
 
rel: deps compile generate reto_server-rel
