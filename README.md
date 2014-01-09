-------------------------------------------------------------------------------
GUT - a graceful test enhancement for Erlang EUnit
-------------------------------------------------------------------------------
Version 0.1
Release date: 01/09/2014
-------------------------------------------------------------------------------
Project state:
expermental
-------------------------------------------------------------------------------
Credits
	Zhu Zheng (zuze.me@gmail.com)
-------------------------------------------------------------------------------
Project description

This project re-orginizes and enhances Erlang unit test 
-------------------------------------------------------------------------------
Dependencies:


-------------------------------------------------------------------------------
Documentation

-------------------------------------------------------------------------------
How to write gut test suite file

A gut test suite file should be TestSuiteName plus the suffix _gut.
All testcases in gut can be originized in different groups
Each test suite has its own setup and cleanup functions
Each group has its own setup and cleanup functions.
Each testcase has its own setup and cleanup functions.
The gut test suite file should have following callback functions

1. init_per_suite/0
Spec:
  -spec init_per_suite() -> SuiteConfig :: any().

Description:
  The setup function of test suite
  Users can do some global initialization for this test suite in this function.
  It returns SuiteConfig which is  passed to init_per_group/2 and end_per_suite/1
  If there is no init_per_suite/0 in the test suite file. The default one returns ok.

2. end_per_suite/1
Spec:
  -spec end_per_suite(SuiteConfig :: any()) -> any().

Description:
  The cleanup function of test suite
  Users can clean up the global initialization in this function

3. init_per_group/2
Spec:
  -spec init_per_group(GroupName :: atom(), SuiteConfig :: any()) 
		    -> GroupConfig :: any().

Description:
  The setup function of each group
  Users can do group initialization in this function.
  It returns GroupConfig which is passed to init_per_testsuite/2 and end_per_group/2.
  If there is no init_per_group/2 in the test suite file. The default one returns ok.

4. end_per_group/2
Spec:
  -spec end_per_group(GroupName :: atom(), GroupConfig :: any ()) -> any().

Description:
  The cleanup function of each group
  Users can do group cleanup in this function.

5. init_per_testcase/2
Spec:
  -spec init_per_testcase(TestcaseName :: atom(), GroupConfig :: any()) 
		       -> TestcaseConfig :: any().

Description:
  The setup function of each testsuite
  It returns TestcaseConfig which is passed to testcase/2.
  If there is no init_per_testcase/2 in the test suite file. The default one returns ok

6. end_per_testcase/2
Spec:
  -spec end_per_testcase(TestcaseName :: atom(), TestcaseConfig :: any()) -> any().

Description:
  The cleanup function of each testsuite

7. groups/0
Spec:
  -spec groups() -> [GroupName :: atom()].

Description:
  A list contains all groups.
  If there is no groups/0 and testcases_per_group/1 in the test suite file, 
a default group will be created.

8. testcases_per_group/1
Spec:
  -spec testcases_per_group(GroupName :: atom()) 
       -> [TestcaseName :: atom() | {TestcaseName :: atom(), 
                                         [{title, Title :: string() 
                                       | {timeout, Timeout :: integer()}}]}]..

Description:
  Returns all testcases in the group
  There are 2 options(Title and Timeout) can be added here.
  If there is no groups/0 and testcases_per_group/1 in the test suite file, 
all testcases will be put into default group.


9. testcase/2
Spec:
  -spec testcase(TestcaseName :: atom(), TestcaseConfig :: any()) -> any().

Description:
  The testcases to run

-------------------------------------------------------------------------------
How to run gut test suite

1. Run all testcases 
gut:test(TestSuite).

2. Run specific groups
gut:test(TestSuite, [GroupName]).

3. Run specific groups and testcases
gut:test(TestSuite, [GroupName], [TestcaseName]).

4. Run with EUnit options
gut:test(TestSuite, [GroupName], [TestcaseName], Options).


