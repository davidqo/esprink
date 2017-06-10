REBAR=rebar
ERLANG=erl
BIN_DIR=bin

all: deps compile testfile

deps: 
	$(REBAR) get-deps

compile:
	$(REBAR) compile

testfile:
	dd bs=1MB count=1 if="/dev/zero" of=testfile

run:
	$(BIN_DIR)/esprink_server

clean:
	$(REBAR) clean

.PHONY: all deps compile run clean testfile
