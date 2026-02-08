%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% test_runner.pl - Unit Test Runner for Prodigy SSG
%%
%% Uses SWI-Prolog's built-in testing framework.

:- use_module(library(plunit)).
:- use_module(library(lists)).

%% Load modules under test
:- use_module('../../engine/src/prodigy_core').
:- use_module('../../engine/src/prodigy_markdown').
:- use_module('../../engine/src/prodigy_frontmatter').
:- use_module('../../engine/src/prodigy_template').

%% ============================================================================
%% Markdown Parser Tests
%% ============================================================================

:- begin_tests(markdown).

test(header_h1) :-
    markdown_to_html("# Hello World\n", Html),
    sub_string(Html, _, _, _, "<h1>Hello World</h1>").

test(header_h2) :-
    markdown_to_html("## Second Level\n", Html),
    sub_string(Html, _, _, _, "<h2>Second Level</h2>").

test(paragraph) :-
    markdown_to_html("This is a paragraph.\n", Html),
    sub_string(Html, _, _, _, "<p>This is a paragraph.</p>").

test(unordered_list) :-
    markdown_to_html("- Item 1\n- Item 2\n", Html),
    sub_string(Html, _, _, _, "<ul>"),
    sub_string(Html, _, _, _, "<li>Item 1</li>"),
    sub_string(Html, _, _, _, "<li>Item 2</li>").

test(code_block) :-
    markdown_to_html("```prolog\nfoo(X) :- bar(X).\n```\n", Html),
    sub_string(Html, _, _, _, "<pre><code"),
    sub_string(Html, _, _, _, "language-prolog").

test(inline_code) :-
    parse_inline("Use `code` here", Html),
    sub_string(Html, _, _, _, "<code>code</code>").

test(bold) :-
    parse_inline("This is **bold** text", Html),
    sub_string(Html, _, _, _, "<strong>bold</strong>").

test(italic) :-
    parse_inline("This is *italic* text", Html),
    sub_string(Html, _, _, _, "<em>italic</em>").

test(link) :-
    parse_inline("[click here](https://example.com)", Html),
    sub_string(Html, _, _, _, "<a href=\"https://example.com\">click here</a>").

test(image) :-
    parse_inline("![alt text](image.png)", Html),
    sub_string(Html, _, _, _, "<img src=\"image.png\" alt=\"alt text\">").

:- end_tests(markdown).

%% ============================================================================
%% Frontmatter Parser Tests
%% ============================================================================

:- begin_tests(frontmatter).

test(simple_frontmatter) :-
    Content = "---\ntitle: Test Post\ndate: 2025-01-15\n---\nBody content",
    parse_frontmatter(Content, Fm, Body),
    get_dict(title, Fm, "Test Post"),
    get_dict(date, Fm, "2025-01-15"),
    sub_string(Body, _, _, _, "Body content").

test(draft_detection) :-
    Content = "---\ntitle: Draft\ndraft: true\n---\nContent",
    parse_frontmatter(Content, Fm, _),
    is_draft(Fm).

test(get_title) :-
    Fm = frontmatter{title: "My Title", date: "2025-01-01"},
    get_title(Fm, Title),
    Title = "My Title".

test(get_tags) :-
    Fm = frontmatter{tags: ["prolog", "ssg"]},
    get_tags(Fm, Tags),
    Tags = ["prolog", "ssg"].

test(missing_field_default) :-
    Fm = frontmatter{},
    get_title(Fm, Title),
    Title = "Untitled".

:- end_tests(frontmatter).

%% ============================================================================
%% Template Engine Tests
%% ============================================================================

:- begin_tests(template).

test(variable_substitution) :-
    render("Hello {{ name }}!", _{name: "World"}, Output),
    Output = "Hello World!".

test(unescaped_html) :-
    render("{{{ html }}}", _{html: "<strong>Bold</strong>"}, Output),
    sub_string(Output, _, _, _, "<strong>Bold</strong>").

test(escaped_html) :-
    render("{{ html }}", _{html: "<script>alert('xss')</script>"}, Output),
    sub_string(Output, _, _, _, "&lt;script&gt;").

test(section_true) :-
    render("{{# show }}visible{{/ show }}", _{show: true}, Output),
    sub_string(Output, _, _, _, "visible").

test(section_false) :-
    render("{{# show }}visible{{/ show }}", _{show: false}, Output),
    Output = "".

test(inverted_section) :-
    render("{{^ items }}No items{{/ items }}", _{items: []}, Output),
    sub_string(Output, _, _, _, "No items").

test(list_iteration) :-
    render("{{# items }}{{ name }} {{/ items }}", _{items: [_{name: "A"}, _{name: "B"}]}, Output),
    sub_string(Output, _, _, _, "A"),
    sub_string(Output, _, _, _, "B").

:- end_tests(template).

%% ============================================================================
%% Core Engine Tests
%% ============================================================================

:- begin_tests(core).

test(version) :-
    version(V),
    string(V).

test(output_filename_md) :-
    output_filename('test.md', Output),
    Output = 'test.html'.

test(output_filename_markdown) :-
    output_filename('page.markdown', Output),
    Output = 'page.html'.

test(parse_content_missing_file) :-
    process_content('/nonexistent/file.md', Result),
    get_dict(message, Result, _).

:- end_tests(core).

%% ============================================================================
%% Test Runner Entry Point
%% ============================================================================

run_tests :-
    format("~n=== Prodigy SSG Unit Tests ===~n~n"),
    run_tests(markdown),
    run_tests(frontmatter),
    run_tests(template),
    run_tests(core),
    format("~n=== All Unit Tests Complete ===~n").
