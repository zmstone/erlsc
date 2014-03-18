all: compile

compile:
	./rebar compile

ct: compile
	./rebar ct skip_deps=true

clean:
	./rebar clean
