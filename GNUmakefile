all: *.erl *.hrl lex.xrl grm.yrl
	erl -compile *.erl
	erl -pa ebin -eval "lexgrm:start(), halt()" -noshell -detached

clean:
	rm -f *.beam grm.erl lex.erl

run_tests: all
	erl +P 1000000 -noshell -eval "eunit:test(test_client), halt()"

run_ping_tests: all
	erl +P 1000000 -noshell -eval "eunit:test({test,test_client,ping}), halt()"

run_robustness_tests: all
	erl +P 1000000 -noshell -eval "eunit:test({timeout, 10, {test,test_client,robustness_channel}}), halt()"
	erl +P 1000000 -noshell -eval "eunit:test({timeout, 10, {test,test_client,robustness_server}}), halt()"

run_concurrency_tests: all
	erl +P 1000000 -noshell -eval "eunit:test({timeout, 10, {test,test_client,process_usage_test}}), halt()"

PERFTESTS = "[\
{timeout, 60, {test,test_client,many_users_one_channel}},\
{timeout, 60, {test,test_client,many_users_many_channels}}\
]"

run_perf_tests: all
	erl +P 1000000 -noshell -eval "eunit:test("${PERFTESTS}"), halt()"

run_distributed_tests: all
	-killall beam.smp 2>/dev/null
	erl -noshell -name "testsuite@127.0.0.1" -setcookie dchat -eval "eunit:test(test_remote), halt()"
