compile:
	./rebar compile
 
deps:
	./rebar get-deps
 
generate:
	cd rel
	rm -rf reto_server
	../rebar generate
	chmod a+x reto_server/bin/reto_server

console:
	. rel/reto_server/bin/reto_server console
 
rel: deps compile generate reto_server-rel
