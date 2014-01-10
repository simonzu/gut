-module(example_no_group_gut).

-export([init_per_suite/0,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 testcase/2]).

init_per_suite() ->
    init.

end_per_suite(init) ->
    ok.

init_per_testcase(testcase1, _) ->
    testcase_config_1;
init_per_testcase(testcase2, _) ->
    testcase_config_2.

end_per_testcase(testcase1, testcase_config_1) ->
    ok;
end_per_testcase(testcase2, testcase_config_2) ->
    ok.

testcase(testcase1, testcase_config_1) ->
    ok;
testcase(testcase2, testcase_config_2) ->
    ok.



