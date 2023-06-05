ERL ?= erl
APP := jsonpath
REBAR := rebar3

.PHONY: deps

all: deps xref
	$(REBAR) compile

deps:
	$(REBAR) get-deps

xref:
	$(REBAR) xref

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit

distclean: clean
	$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
