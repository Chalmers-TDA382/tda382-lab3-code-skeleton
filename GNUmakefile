all: gui client server cchat

cchat:  cchat.erl server.beam gui.beam
	erl -compile cchat.erl

server: server.erl defs.hrl genserver
	erl -compile server.erl

client: client.erl defs.hrl lexgrm genserver
	erl -compile client.erl

gui: gui.erl genserver
	erl -compile gui.erl

genserver: genserver.erl
	erl -compile genserver

lexgrm : lex.xrl grm.yrl lexgrm.erl
	 erl -compile lexgrm.erl
	 erl -pa ebin -eval "lexgrm:start()" -noshell -detached

clean:
	rm -f *.beam


run_tests: tests all
	erl +P 1000000 -eval "eunit:test(test_client), halt()"

PERFTESTS = "[\
{timeout, 60, {test,test_client,many_users_one_channel}},\
{timeout, 60, {test,test_client,many_users_many_channels}}\
]"

run_perf_tests: tests all
	echo "\n\033[32m=== Running with 4 cores === \033[0m\n"
	erl -smp +S 4 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo "\n\033[32m=== Running with 2 cores === \033[0m\n"
	erl -smp +S 2 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo "\n\033[32m=== Running with 1 core  === \033[0m\n"
	erl -smp +S 1 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"

run_distributed_tests: tests all
	erl -name "testsuite@127.0.0.1" -eval "eunit:test(test_remote), halt()"

tests: test_client.beam dummy_gui.beam test_remote.beam

test_client.beam : test_client.erl
	erl -compile test_client.erl

test_remote.beam : test_remote.erl
	erl -compile test_remote.erl

dummy_gui.beam : dummy_gui.erl
	erl -compile dummy_gui.erl
