.PHONY: all compile clean

all: compile
	@erl -pa ${PWD}/_build/default/lib/*/ebin -pa ${PWD}/_checkouts/*/ebin -name sandbox -hidden -boot start_sasl -run sand

compile:
	@rebar3 compile

clean:
	@rebar3 clean