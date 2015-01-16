all: gui.beam helper.beam lexgrm.beam cchat.beam server.beam client.beam

gui.beam: gui.erl helper.beam
	erl -compile gui.erl

helper.beam: helper.erl
	erl -compile helper.erl

lexgrm.beam: lexgrm.erl lex.erl grm.erl
	erl -compile lex.erl
	erl -compile grm.erl
	erl -compile lexgrm.erl
#	erl -pa ebin -eval "lexgrm:start()" -noshell -detached

cchat.beam: cchat.erl server.beam gui.beam helper.beam
	erl -compile cchat.erl

server.beam: server.erl defs.hrl helper.beam
	erl -compile server.erl

client.beam: client.erl defs.hrl lexgrm.beam helper.beam
	erl -compile client.erl

# ----------------------------------------------------------------------------

clean:
	rm -f *.beam

run_tests: all tests
	erl +P 1000000 -eval "eunit:test(test_client), halt()"

run_ping_tests: all tests
	erl +P 1000000 -eval "eunit:test({test,test_client,ping}), halt()"

PERFTESTS = "[\
{timeout, 60, {test,test_client,many_users_one_channel}},\
{timeout, 60, {test,test_client,many_users_many_channels}}\
]"

run_perf_tests: all tests
	echo "\n\033[32m=== Running with 4 cores === \033[0m\n"
	erl -smp +S 4 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo "\n\033[32m=== Running with 2 cores === \033[0m\n"
	erl -smp +S 2 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo "\n\033[32m=== Running with 1 core  === \033[0m\n"
	erl -smp +S 1 +P 1000000 -eval "eunit:test("$(PERFTESTS)"),halt()"

# run_distributed_tests: all tests
# 	-killall beam.smp 2>/dev/null
# 	erl -name "testsuite@127.0.0.1" -eval "eunit:test(test_remote), halt()"

tests: dummy_gui.beam test_client.beam # test_remote.beam

dummy_gui.beam: dummy_gui.erl
	erl -compile dummy_gui.erl

test_client.beam: test_client.erl
	erl -compile test_client.erl

# test_remote.beam: test_remote.erl
# 	erl -compile test_remote.erl
