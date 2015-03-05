all: *.erl *.hrl lex.xrl grm.yrl
	erl -compile *.erl
	erl -pa ebin -eval "lexgrm:start(), halt()" -noshell -detached

clean:
	rm -f *.beam

run_tests: all
	erl +P 1000000 -eval "eunit:test(test_client), halt()"

run_ping_tests: all
	erl +P 1000000 -eval "eunit:test({test,test_client,ping}), halt()"

PERFTESTS = "[\
{timeout, 60, {test,test_client,many_users_one_channel}},\
{timeout, 60, {test,test_client,many_users_many_channels}}\
]"

run_perf_tests: all
	echo -e "\n\033[32m=== Running with 4 cores ===\033[0m"
	erl -smp +S 4 +P 1000000 -noshell -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo -e "\n\033[32m=== Running with 2 cores ===\033[0m"
	erl -smp +S 2 +P 1000000 -noshell -eval "eunit:test("$(PERFTESTS)"),halt()"
	echo -e "\n\033[32m=== Running with 1 core  ===\033[0m"
	erl -smp +S 1 +P 1000000 -noshell -eval "eunit:test("$(PERFTESTS)"),halt()"
