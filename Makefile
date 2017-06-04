REBAR=rebar
ERLANG=erl

all: deps compile testfile

deps: 
	$(REBAR) get-deps

compile:
	$(REBAR) compile

testfile:
	dd bs=1MB count=1 if="/dev/zero" of=testfile

run:
	$(ERLANG) -pa ebin/ deps/*/ebin -smp -s esprink

clean:
	$(REBAR) clean

.PHONY: all deps compile run clean testfile
