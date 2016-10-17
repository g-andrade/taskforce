REBAR = $(shell command -v rebar || echo ./rebar)
DEPS_PLT=./.deps_plt
DEPS=erts kernel stdlib

.PHONY: all get-deps compile clean dialyze xref

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

#test: compile
#	@ERL_AFLAGS="-config test/taskforce_tests.app.config" $(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

$(DEPS_PLT):
	@echo Building $(DEPS_PLT)
	dialyzer --build_plt \
	  --output_plt $(DEPS_PLT) \
	  --apps $(DEPS)
#-r deps \

dialyze: compile $(DEPS_PLT)
	dialyzer --fullpath \
		--src src \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
		-r ebin \
		--plt $(DEPS_PLT)

xref:
	@$(REBAR) xref

doc: compile
	./scripts/hackish_make_docs.sh
