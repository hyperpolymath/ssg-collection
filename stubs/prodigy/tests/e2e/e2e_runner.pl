%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% e2e_runner.pl - End-to-End Test Runner
%%
%% Tests the full site generation pipeline.

:- use_module(library(filesex)).
:- use_module(library(lists)).

:- use_module('../../engine/src/prodigy_core').

%% Test fixtures directory
fixtures_dir('tests/fixtures').
output_dir('tests/output').

%% ============================================================================
%% E2E Test Cases
%% ============================================================================

%% run_e2e_tests/0
%% Run all end-to-end tests
run_e2e_tests :-
    format("~n=== Prodigy SSG E2E Tests ===~n~n"),
    setup_test_env,
    ( run_all_e2e_tests(Results)
    -> report_results(Results),
       cleanup_test_env,
       all_passed(Results)
    ;  cleanup_test_env,
       fail
    ).

%% setup_test_env/0
%% Prepare test environment
setup_test_env :-
    output_dir(OutDir),
    ( exists_directory(OutDir) -> delete_directory_and_contents(OutDir) ; true ),
    make_directory_path(OutDir),
    create_test_fixtures.

%% cleanup_test_env/0
%% Clean up after tests
cleanup_test_env :-
    output_dir(OutDir),
    ( exists_directory(OutDir) -> delete_directory_and_contents(OutDir) ; true ).

%% create_test_fixtures/0
%% Create test content files
create_test_fixtures :-
    fixtures_dir(Dir),
    make_directory_path(Dir),
    % Create test markdown file
    atomic_list_concat([Dir, '/test-page.md'], Path1),
    open(Path1, write, S1),
    write(S1, '---\ntitle: Test Page\ndate: 2025-01-15\n---\n\n# Hello\n\nThis is a test.\n'),
    close(S1),
    % Create another test file
    atomic_list_concat([Dir, '/about.md'], Path2),
    open(Path2, write, S2),
    write(S2, '---\ntitle: About\n---\n\n## About Us\n\nWelcome to our site.\n'),
    close(S2).

%% run_all_e2e_tests(-Results)
%% Run each test and collect results
run_all_e2e_tests(Results) :-
    findall(
        result(Name, Status),
        ( e2e_test(Name, Goal),
          ( call(Goal) -> Status = pass ; Status = fail )
        ),
        Results
    ).

%% E2E test definitions
e2e_test(build_site, test_build_site).
e2e_test(output_files_created, test_output_files_created).
e2e_test(html_structure, test_html_structure).
e2e_test(frontmatter_applied, test_frontmatter_applied).

%% test_build_site/0
%% Test that site builds without errors
test_build_site :-
    fixtures_dir(Input),
    output_dir(Output),
    Config = config{input_dir: Input, output_dir: Output, templates_dir: 'templates', base_url: '/'},
    build_site(Config, Output),
    exists_directory(Output).

%% test_output_files_created/0
%% Test that expected output files exist
test_output_files_created :-
    output_dir(OutDir),
    atomic_list_concat([OutDir, '/test-page.html'], File1),
    atomic_list_concat([OutDir, '/about.html'], File2),
    exists_file(File1),
    exists_file(File2).

%% test_html_structure/0
%% Test that output is valid HTML
test_html_structure :-
    output_dir(OutDir),
    atomic_list_concat([OutDir, '/test-page.html'], File),
    read_file_to_string(File, Content, []),
    sub_string(Content, _, _, _, "<!DOCTYPE html>"),
    sub_string(Content, _, _, _, "<html"),
    sub_string(Content, _, _, _, "</html>").

%% test_frontmatter_applied/0
%% Test that frontmatter is used in output
test_frontmatter_applied :-
    output_dir(OutDir),
    atomic_list_concat([OutDir, '/test-page.html'], File),
    read_file_to_string(File, Content, []),
    sub_string(Content, _, _, _, "<title>Test Page</title>").

%% report_results(+Results)
%% Print test results
report_results(Results) :-
    format("~nTest Results:~n"),
    format("=============~n"),
    forall(
        member(result(Name, Status), Results),
        format("  ~w: ~w~n", [Name, Status])
    ),
    include([result(_, pass)]>>true, Results, Passed),
    include([result(_, fail)]>>true, Results, Failed),
    length(Passed, PassCount),
    length(Failed, FailCount),
    format("~nPassed: ~w, Failed: ~w~n", [PassCount, FailCount]).

%% all_passed(+Results)
%% Check if all tests passed
all_passed(Results) :-
    \+ member(result(_, fail), Results).
