-module(example_normal_gut).
%%%============================================================================
%%% #1.1   EXPORT LISTS
%%%============================================================================
-compile(export_all).
%%%============================================================================
%%% #1.2   INCLUDES
%%%============================================================================

-include_lib("eunit/include/eunit.hrl").

init_per_suite() ->
    init_config.

end_per_suite(init_config) ->
    ok.

init_per_group(group1, init_config) ->
    group_config_1;
init_per_group(group2, init_config) ->
    group_config_2.

end_per_group(group1, group_config_1) ->
    ok;
end_per_group(group2, group_config_2) ->
    ok.

groups() ->
    [group1, group2].

init_per_testcase(testcase1, group_config_1) ->
    test_config_1_1;
init_per_testcase(testcase1, group_config_2) ->
    test_config_1_2;
init_per_testcase(testcase2, group_config_1) ->
    test_config_2;
init_per_testcase(testcase3, group_config_2) ->
    test_config_3.

end_per_testcase(testcase1, test_config_1_1) ->
    ok;
end_per_testcase(testcase1, test_config_1_2) ->
    ok;
end_per_testcase(testcase2, test_config_2) ->
    ok;
end_per_testcase(testcase3, test_config_3) ->
    ok.

testcases_per_group(group1) ->
    [{testcase1, [{title, "testcase1 in group1"}]}, {testcase2, [{timeout, 50}]}];
testcases_per_group(group2) ->
    [{testcase1, [{title, "testcase1 in group2"}]}, testcase3].


testcase(testcase1, test_config_1_1) ->
    ok;
testcase(testcase1, test_config_1_2) ->
    ok;
testcase(testcase2, test_config_2) ->
    timer:sleep(10 * 1000);
testcase(testcase3, test_config_3) ->
    ok.








