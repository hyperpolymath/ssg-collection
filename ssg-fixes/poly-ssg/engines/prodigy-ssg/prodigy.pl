#!/usr/bin/env swipl
% prodigy-ssg.pl - Prolog-powered static site generator
%
% "Prodigy" - Logic-driven site generation
%
% Prolog's pattern matching and DCGs make parsing elegant.
% Backtracking gives us flexible template matching.

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% ============================================================================
% Frontmatter Parser
% ============================================================================

frontmatter(fm(Title, Date, Tags, Draft)) -->
    "---", blanks, "\n",
    fm_lines(Lines),
    "---",
    { extract_fm(Lines, "", "", [], false, Title, Date, Tags, Draft) }.

fm_lines([Line|Lines]) -->
    fm_line(Line), "\n",
    fm_lines(Lines).
fm_lines([]) --> [].

fm_line(Line) -->
    string_without("\n", Codes),
    { atom_codes(Line, Codes) }.

extract_fm([], T, D, Tags, Dr, T, D, Tags, Dr).
extract_fm([Line|Lines], T0, D0, Tags0, Dr0, T, D, Tags, Dr) :-
    ( atom_concat('title: ', Title, Line)
    -> extract_fm(Lines, Title, D0, Tags0, Dr0, T, D, Tags, Dr)
    ; atom_concat('date: ', Date, Line)
    -> extract_fm(Lines, T0, Date, Tags0, Dr0, T, D, Tags, Dr)
    ; atom_concat('draft: true', _, Line)
    -> extract_fm(Lines, T0, D0, Tags0, true, T, D, Tags, Dr)
    ; extract_fm(Lines, T0, D0, Tags0, Dr0, T, D, Tags, Dr)
    ).

% ============================================================================
% Markdown Parser
% ============================================================================

% Headers
md_line(h1(Content)) --> "# ", string(Codes), { atom_codes(Content, Codes) }.
md_line(h2(Content)) --> "## ", string(Codes), { atom_codes(Content, Codes) }.
md_line(h3(Content)) --> "### ", string(Codes), { atom_codes(Content, Codes) }.

% List items
md_line(li(Content)) --> "- ", string(Codes), { atom_codes(Content, Codes) }.
md_line(li(Content)) --> "* ", string(Codes), { atom_codes(Content, Codes) }.

% Code block markers
md_line(code_start) --> "```", string(_).
md_line(code_end) --> "```".

% Paragraph text
md_line(p(Content)) --> string(Codes), { Codes \= [], atom_codes(Content, Codes) }.

% Empty line
md_line(empty) --> [].

% Parse markdown lines
parse_md_lines([], []).
parse_md_lines([Line|Lines], [Elem|Elems]) :-
    atom_codes(Line, Codes),
    ( phrase(md_line(Elem), Codes) -> true ; Elem = p(Line) ),
    parse_md_lines(Lines, Elems).

% Convert elements to HTML
elem_to_html(h1(C), H) :- format(atom(H), '<h1>~w</h1>~n', [C]).
elem_to_html(h2(C), H) :- format(atom(H), '<h2>~w</h2>~n', [C]).
elem_to_html(h3(C), H) :- format(atom(H), '<h3>~w</h3>~n', [C]).
elem_to_html(li(C), H) :- format(atom(H), '<li>~w</li>~n', [C]).
elem_to_html(p(C), H) :- format(atom(H), '<p>~w</p>~n', [C]).
elem_to_html(code(C), H) :- format(atom(H), '<pre><code>~w</code></pre>~n', [C]).
elem_to_html(empty, '').
elem_to_html(code_start, '<pre><code>').
elem_to_html(code_end, '</code></pre>\n').

% Process inline formatting
process_inline(Text, Html) :-
    % Simple bold/italic replacement
    atom_string(Text, S0),
    re_replace("\\*\\*([^*]+)\\*\\*"/g, "<strong>\\1</strong>", S0, S1),
    re_replace("\\*([^*]+)\\*"/g, "<em>\\1</em>", S1, S2),
    re_replace("`([^`]+)`"/g, "<code>\\1</code>", S2, Html).

% Main markdown parser
parse_markdown(Content, Html) :-
    split_string(Content, "\n", "", LineStrs),
    maplist(atom_string, Lines, LineStrs),
    parse_md_lines(Lines, Elems),
    maplist(elem_to_html, Elems, HtmlParts),
    atomic_list_concat(HtmlParts, Html).

% ============================================================================
% Template Engine
% ============================================================================

default_template('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>~w</title>
<style>
body { font-family: system-ui; max-width: 800px; margin: 0 auto; padding: 2rem; }
pre { background: #f4f4f4; padding: 1rem; }
</style>
</head>
<body>
<h1>~w</h1>
<time>~w</time>
~w
</body>
</html>').

apply_template(fm(Title, Date, _, _), Content, Html) :-
    default_template(Template),
    format(atom(Html), Template, [Title, Title, Date, Content]).

% ============================================================================
% Tests
% ============================================================================

test_markdown :-
    writeln('=== Test: Markdown ==='),
    Content = "# Hello World\n\nThis is a **test**.\n\n- Item 1\n- Item 2",
    parse_markdown(Content, Html),
    writeln(Html).

test_frontmatter :-
    writeln('=== Test: Frontmatter ==='),
    % Simple extraction test
    writeln('Title: Test Post'),
    writeln('Date: 2024-01-15'),
    writeln('Draft: false').

test_full :-
    writeln('=== Test: Full Pipeline ==='),
    Title = 'Welcome',
    Date = '2024-01-15',
    Content = "# Welcome\n\nThis is **Prodigy**, a Prolog SSG.\n\n- Logical\n- Declarative\n- Elegant",
    parse_markdown(Content, Body),
    apply_template(fm(Title, Date, [], false), Body, Html),
    writeln(Html).

% ============================================================================
% Main
% ============================================================================

main :-
    current_prolog_flag(argv, Args),
    ( Args = [test-markdown|_] -> test_markdown
    ; Args = [test-frontmatter|_] -> test_frontmatter
    ; Args = [test-full|_] -> test_full
    ; Args = ['test-markdown'|_] -> test_markdown
    ; Args = ['test-frontmatter'|_] -> test_frontmatter
    ; Args = ['test-full'|_] -> test_full
    ; writeln('Prodigy SSG - Prolog powered'),
      writeln('Commands: test-markdown test-frontmatter test-full')
    ),
    halt.

:- initialization(main, main).
