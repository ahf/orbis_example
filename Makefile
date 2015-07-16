REBAR = ./rebar3

all: compile

compile:
	@$(REBAR) compile

rel:
	@$(REBAR) release

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check:
	@$(REBAR) do ct -v, cover -v

shell:
	@$(REBAR) shell

console:
	./_build/default/rel/orbis_example/bin/orbis_example console

.PHONY: compile rel clean dialyzer check shell console
