.PHONY: all compile clean

all: compile check

dev: compile with_sandbox
	@erl -pa ${PWD}/_build/default/lib/*/ebin -pa ${PWD}/sandbox/_build/default/lib/*/ebin -name sandbox -hidden -boot start_sasl -run sand

compile:
	@rebar3 compile

with_sandbox:
	@cd ${PWD}/sandbox; rebar3 compile

check:
	@rebar3 eunit

clean:
	@rebar3 clean