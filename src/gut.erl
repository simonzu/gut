%%% The MIT License (MIT)
%%% 
%%% Copyright (c) 2014 ZHU ZHENG <zuze.me@gmail.com>
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%%% this software and associated documentation files (the "Software"), to deal in
%%% the Software without restriction, including without limitation the rights to
%%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%% the Software, and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%% FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%% COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%% IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-----------------------------------------------------------------------------------

-module(gut).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([test/1, test/2, test/4]).

%% gut callbacks
-export([init_per_suite/0,
	 end_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 groups/0,
	 testcases_per_group/1,
	 testcase/2]).

%% gut function
-export([gut_main/0]).

%%====================================================================
%% Includes
%%====================================================================
-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Defines
%%====================================================================
-define(FUNCTION(Name, ArgNum, Body),
		{function, ?LINE, Name, ArgNum, Body}).

-define(INTEGER(Name), {integer, ?LINE, Name}).

-define(ATOM(Name), {atom, ?LINE, Name}).

-define(VAR(Name), {var, ?LINE, Name}).

-define(CONS(H, T), {cons, ?LINE, H, T}).

-define(NIL(), {nil, ?LINE}).

-define(CLAUSE(Args, Body), {clause, ?LINE, Args, [], Body}).

-define(ABSTRACT_CODE_GUARD(Guard), {abstract_code, Guard}).

-define(FUNCTION_GUARD(Name, ArgNum, Body), {function, _, Name, ArgNum, Body}).

-define(ATOM_GUARD(Name), {atom, _, Name}).

-define(ATTRIBUTE_GUARD(Name, Value), {attribute, _, Name, Value}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test(Module)
%% Description: Executes the testcases of Module
%%--------------------------------------------------------------------

test(Module) ->
    test(Module,[]).

test(Module, Options) ->
    test(Module, Options, all, all).

test(Module, Options,  Groups, Testcases) ->
    TestModule = case lists:suffix("_tests", atom_to_list(Module)) of
		     true ->
			 Module;
		     false ->
			 list_to_atom(atom_to_list(Module) ++ "_tests")
		 end,

    {_Mod,_Beam,FilePath} = code:get_object_code(TestModule),
    {ok, {_, [?ABSTRACT_CODE_GUARD({_, TestModuleForms0})]}} =
	beam_lib:chunks(FilePath, [abstract_code]),

    TemplateForms = abstract_code(),

    TestModuleForms1 = add_template_functions(TemplateForms, lists:reverse(TestModuleForms0)),

    TestModuleForms2 = add_missing_functions(TemplateForms, TestModuleForms1),

    TestModuleForms3 = eunit_test_generator(TemplateForms, TestModuleForms2),

    TestModuleForms4 = set_missing_group(TestModuleForms3),

    TestModuleForms5 = groups_to_test(Groups, TestModuleForms4),

    TestModuleForms6 = testcases_to_test(Testcases, TestModuleForms5),

    {ok, TestModule, Binary, _Warning} = 
	compile:forms(lists:reverse(TestModuleForms6), [return_errors, return_warnings]),

    code:purge(TestModule),
    code:load_binary(TestModule,FilePath,Binary),

    eunit:test(TestModule, Options).

abstract_code() ->
    {_Mod,_Beam,FilePath} = code:get_object_code(?MODULE),
    {ok, {_, [?ABSTRACT_CODE_GUARD({_, AC})]}} =
	beam_lib:chunks(FilePath, [abstract_code]),
    [R || ?FUNCTION_GUARD(_, _, _) = R <- AC].


%%--------------------------------------------------------------------
%% Function: add_template_functions(TemplateForms, TestModuleForms)
%% Description: Add all template functions which is needed by gut
%%--------------------------------------------------------------------
add_template_functions(TemplateForms, TestModuleForms) ->
    add_template_function(gut_group, TemplateForms,
    add_template_function(assemble_gut_testcase, TemplateForms,
    add_template_function(gut_testcases, TemplateForms,
    add_template_function(gut_testcase_template, TemplateForms, TestModuleForms)))).


add_template_function(Function, TemplateForms, TestModuleForms) ->
    add_template_function_from_form(Function, TemplateForms, TestModuleForms).


add_template_function_from_form(Function, [], _TestModuleForms) ->
    throw({template_function_not_found, Function});
add_template_function_from_form(Function, [?FUNCTION_GUARD(Function, _, _) = FunctionForm | _], 
				TestModuleForms) ->
    [FunctionForm | TestModuleForms];
add_template_function_from_form(Function, [_ | TemplateForms], TestModuleForms) ->
    add_template_function_from_form(Function, TemplateForms, TestModuleForms).

%%--------------------------------------------------------------------
%% Function: add_missing_functions(TemplateForms, TestModuleForms)
%% Description: Some default functions which will be added when missing in test module
%%--------------------------------------------------------------------
add_missing_functions(TemplateForms, TestModuleForms) ->
    add_missing_function(end_per_testcase, TemplateForms,
    add_missing_function(init_per_testcase, TemplateForms,
    add_missing_function(end_per_group, TemplateForms,
    add_missing_function(init_per_group, TemplateForms,
    add_missing_function(end_per_suite, TemplateForms,
    add_missing_function(init_per_suite, TemplateForms, TestModuleForms)))))).

add_missing_function(Function, TemplateForms, TestModuleForms) ->
    case lists:any(fun(?FUNCTION_GUARD(Fun, _, _)) ->
		      Fun =:= Function;
		 (_) ->
		      false
	      end, TestModuleForms) of
	true ->
	    TestModuleForms;
	false ->
	    add_template_function(Function, TemplateForms, TestModuleForms)
    end.



%%--------------------------------------------------------------------
%% Function: set_missing_group(TestModuleForms)
%% Description: When the groups/0 function is mssing,
%%              all the testcases will be added to default group
%%--------------------------------------------------------------------
set_missing_group(TestModuleForms) ->
    HasGroups = lists:any(fun(?FUNCTION_GUARD(groups, 0, _)) ->
				  true;
			     (_) ->
				  false
			  end, TestModuleForms),
    case HasGroups of
	false ->
	    AllTestcases = group_all_testcases(TestModuleForms),
	    DefaultGroup = 
		?FUNCTION(groups, 0, [?CLAUSE([], [?CONS(?ATOM(default), ?NIL())])]),

	    [DefaultGroup, AllTestcases | TestModuleForms];
	true ->
	    TestModuleForms
    end.


group_all_testcases(TestModuleForms) ->
    [?FUNCTION_GUARD(test, 2, Clauses)] = 
	lists:filter(fun(?FUNCTION_GUARD(testcase, 2, _)) ->
			     true;
			(_) ->
			     false
		     end, TestModuleForms),
    Testcases = lists:foldl(fun(Clause, Acc) ->
				    case element(3, Clause) of
					[?ATOM_GUARD(Testcase)|_] ->
					    [Testcase | Acc];
					_ ->
					    Acc
				    end
			    end, [], Clauses),
    ?FUNCTION(testcases_per_group, 1, [?CLAUSE([?ATOM(default)], 
						[generate_gut_testcases(Testcases, ?NIL())])]).

generate_gut_testcases([], Const) ->
    Const;

generate_gut_testcases([Testcase | Testcases], Const) ->
    generate_gut_testcases(Testcases, ?CONS(?ATOM(Testcase), Const)).

%%--------------------------------------------------------------------
%% Function: eunit_test_generator(TemplateForms, TestModuleForms)
%% Description: Create the eunit test generator which can be run in eunit
%%--------------------------------------------------------------------
eunit_test_generator(TemplateForms, TestModuleForms) ->
    [?ATTRIBUTE_GUARD(module, TestModule)] = 
	lists:filter(fun(?ATTRIBUTE_GUARD(module, _)) -> 
			     true;
			(_) ->
			     false
		     end, TestModuleForms),

    [?FUNCTION_GUARD(gut_main, 0, Clauses)] = 
	lists:filter(fun(?FUNCTION_GUARD(gut_main, 0, _)) -> 
			     true;
			(_) ->
			     false
		     end, TemplateForms),
    
    MainTest = ?FUNCTION(list_to_atom(atom_to_list(TestModule) ++ "_test_"), 0, Clauses),
    
    [MainTest | TestModuleForms].

%%--------------------------------------------------------------------
%% Function: groups_to_test(GroupNames, TestModuleForms))
%% Description: Add all groups to be tested
%%--------------------------------------------------------------------
groups_to_test(GroupNames, TestModuleForms) ->
    Clauses = case GroupNames of
		  all ->
		      [?CLAUSE([?VAR('_')], [?ATOM(true)])];
		  _GroupNames ->
		      Clauses1 = 
			  lists:map(fun(GroupName) ->
					    ?CLAUSE([?ATOM(GroupName)], [?ATOM(true)])
				    end, GroupNames),
		      lists:reverse([?CLAUSE([?VAR('_')], [?ATOM(false)]) | Clauses1])
	      end,
   
    
    ValidGroup = ?FUNCTION(valid_gut_group, 1, Clauses),
    [ValidGroup | TestModuleForms].

%%--------------------------------------------------------------------
%% Function: groups_to_test(GroupNames, TestModuleForms))
%% Description: Add all testcases to be tested
%%--------------------------------------------------------------------
testcases_to_test(Testcases, TestModuleForms) ->
    Clauses = case Testcases of
		  all ->
		      [?CLAUSE([?VAR('_')], [?ATOM(true)])];
		  _Testcases ->
		      Clauses1 = 
			  lists:map(fun(Testcase) ->
					    ?CLAUSE([?ATOM(Testcase)], [?ATOM(true)])
				    end, Testcases),
		      lists:reverse([?CLAUSE([?VAR('_')], [?ATOM(false)]) | Clauses1])
	      end,
    ValidTestcase = ?FUNCTION(valid_gut_testcase, 1, Clauses),
    [ValidTestcase | TestModuleForms].

%%====================================================================
%% gut callbackes
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init_per_suite()
%% Description: This function will be called before running suite.
%%              This function return a SuiteConfig
%%--------------------------------------------------------------------
init_per_suite() ->
    ok.

%%--------------------------------------------------------------------
%% Function: end_per_suite(SuiteConfig)
%% Description: This function will be called after running suite.
%%--------------------------------------------------------------------
end_per_suite(_) ->
    ok.


%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, TestsuiteConfig)
%% Description: This function will be called before running the testcases in group.
%%              This function return a GroupConfig
%%--------------------------------------------------------------------
init_per_group(_, _) ->
    ok.
%%--------------------------------------------------------------------
%% Function: end_per_group(GroupConfig)
%% Description: This function will be called after running testcases in group.
%%--------------------------------------------------------------------
end_per_group(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestcaseName, GroupConfig)
%% Description: This function will be called before running a testcase.
%%              This function return a TestcaseConfig
%%--------------------------------------------------------------------
init_per_testcase(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestcaseConfig)
%% Description: This function will be called after running a testcase
%%--------------------------------------------------------------------
end_per_testcase(_, _) ->
    ok.
%%--------------------------------------------------------------------
%% Function: groups()
%% Description: This function returns a list of group name
%%--------------------------------------------------------------------
groups() ->
    [default].

%%--------------------------------------------------------------------
%% Function: testcases_per_group(GroupName)
%% Description: This function returns a list of testcase name in the group
%%--------------------------------------------------------------------
testcases_per_group(default) ->
    [].

%%--------------------------------------------------------------------
%% Function: testcase(TestcaseName, TestcaseConfig)
%% Description: The testcase to be implemented
%%--------------------------------------------------------------------
testcase(_, _) ->
    ok.

%%====================================================================
%% gut functions
%%====================================================================

gut_main() ->
    {foreach,
     fun () ->
	     init_per_suite()
     end,
     fun(Config) ->
	     end_per_suite(Config)
     end,
     [fun(Config) ->
	      lists:map(fun(GroupName) -> gut_group(GroupName, Config) end ,groups())
      end]
     
    }.

valid_gut_group(_) ->
    true.

valid_gut_testcase(_) ->
    true.


gut_testcase_template({Testcase, Options1}, GroupConfig) when is_list(Options1) ->
    Options = case lists:keyfind(title, 1, Options1) of
		  false ->
		      [{title, atom_to_list(Testcase)} | Options1];
		  _ ->
		      Options1
	      end,

    case valid_gut_testcase(Testcase) of
	true->
	    {setup,
	     fun() ->
		     init_per_testcase(Testcase ,GroupConfig)
	     end,
	     fun(TestcaseConfig) ->
		     end_per_testcase(Testcase, TestcaseConfig)
	     end,
	     fun(TestcaseConfig) ->
		     assemble_gut_testcase(fun() -> 
						   testcase(Testcase, TestcaseConfig) 
					   end, Options)
	     end
	    };
	false ->
	    []
    end;

gut_testcase_template(Testcase, GroupConfig) when is_atom(Testcase) ->
    gut_testcase_template({Testcase, []}, GroupConfig).

gut_testcases(GroupName, GroupConfig) ->
    lists:map(fun(Testcase) -> 
		      gut_testcase_template(Testcase, GroupConfig) 
	      end, testcases_per_group(GroupName)).

assemble_gut_testcase(Testcase, []) ->
    Testcase;
assemble_gut_testcase(Testcase, [{timeout, Timeout} | Options]) ->
    assemble_gut_testcase({timeout, Timeout, Testcase}, Options);
assemble_gut_testcase(Testcase, [{title, Title} | Options]) ->
    assemble_gut_testcase({Title, Testcase}, Options);
assemble_gut_testcase(Testcase, [_|Options]) ->
    assemble_gut_testcase(Testcase, Options).


gut_group(GroupName, SuiteConfig) ->
    case valid_gut_group(GroupName) of
	true ->
	    {foreach,
	     fun() ->
		     init_per_group(GroupName, SuiteConfig)
	     end,
	     fun(GroupConfig) ->
		     end_per_group(GroupName, GroupConfig)
	     end,
	     [
	      fun(GroupConfig) ->
		      gut_testcases(GroupName, GroupConfig)
	      end
	     ]
	    };
	false ->
	    []
    end.


