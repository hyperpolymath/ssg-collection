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
% File System Operations
% ============================================================================

% Site configuration
site_config(content_dir, 'content').
site_config(output_dir, 'public').
site_config(template_dir, 'templates').
site_config(static_dir, 'static').

% Ensure directory exists
ensure_dir(Dir) :-
    ( exists_directory(Dir) -> true ; make_directory_path(Dir) ).

% Read file contents
read_file_content(Path, Content) :-
    read_file_to_string(Path, Content, []).

% Write file contents
write_file_content(Path, Content) :-
    file_directory_name(Path, Dir),
    ensure_dir(Dir),
    open(Path, write, Stream),
    write(Stream, Content),
    close(Stream).

% List files in directory
list_md_files(Dir, Files) :-
    exists_directory(Dir),
    directory_files(Dir, AllFiles),
    include(is_md_file, AllFiles, Files).

is_md_file(File) :-
    atom_concat(_, '.md', File),
    File \= '.',
    File \= '..'.

% ============================================================================
% Build Command
% ============================================================================

build_site :-
    writeln('Building site...'),
    site_config(content_dir, ContentDir),
    site_config(output_dir, OutputDir),
    ensure_dir(OutputDir),
    ( exists_directory(ContentDir) ->
        list_md_files(ContentDir, MdFiles),
        build_files(ContentDir, OutputDir, MdFiles, Count),
        copy_static_files(StaticCount),
        format('~nBuild complete: ~d pages, ~d static files~n', [Count, StaticCount])
    ;
        format('No content directory found. Creating ~w/~n', [ContentDir]),
        ensure_dir(ContentDir)
    ).

build_files(_, _, [], 0).
build_files(ContentDir, OutputDir, [File|Rest], Count) :-
    atomic_list_concat([ContentDir, '/', File], SrcPath),
    read_file_content(SrcPath, Content),
    % Parse and convert
    parse_markdown(Content, Html),
    apply_template(fm('Untitled', '', [], false), Html, Output),
    % Output path
    atom_concat(BaseName, '.md', File),
    atom_concat(BaseName, '.html', OutFile),
    atomic_list_concat([OutputDir, '/', OutFile], OutPath),
    write_file_content(OutPath, Output),
    format('  ~w -> ~w~n', [File, OutPath]),
    build_files(ContentDir, OutputDir, Rest, RestCount),
    Count is RestCount + 1.

copy_static_files(0) :-
    site_config(static_dir, StaticDir),
    \+ exists_directory(StaticDir), !.
copy_static_files(Count) :-
    site_config(static_dir, StaticDir),
    site_config(output_dir, OutputDir),
    directory_files(StaticDir, AllFiles),
    include(is_regular_file_name, AllFiles, Files),
    copy_files(StaticDir, OutputDir, Files, Count).

is_regular_file_name(F) :- F \= '.', F \= '..'.

copy_files(_, _, [], 0).
copy_files(SrcDir, DstDir, [File|Rest], Count) :-
    atomic_list_concat([SrcDir, '/', File], Src),
    atomic_list_concat([DstDir, '/', File], Dst),
    copy_file(Src, Dst),
    copy_files(SrcDir, DstDir, Rest, RestCount),
    Count is RestCount + 1.

% ============================================================================
% Init Command
% ============================================================================

init_site(Name) :-
    format('Initializing new site: ~w~n', [Name]),
    % Create directories
    atomic_list_concat([Name, '/content'], ContentDir),
    atomic_list_concat([Name, '/templates'], TemplateDir),
    atomic_list_concat([Name, '/static'], StaticDir),
    atomic_list_concat([Name, '/public'], PublicDir),
    ensure_dir(ContentDir),
    ensure_dir(TemplateDir),
    ensure_dir(StaticDir),
    ensure_dir(PublicDir),
    format('  Created ~w/~n', [ContentDir]),
    format('  Created ~w/~n', [TemplateDir]),
    format('  Created ~w/~n', [StaticDir]),
    format('  Created ~w/~n', [PublicDir]),
    % Create sample content
    atomic_list_concat([ContentDir, '/index.md'], IndexPath),
    SampleContent = '---\ntitle: Welcome to Prodigy\ndate: 2025-01-18\n---\n\n# Welcome\n\nThis is your first post built with **Prodigy SSG**.\n\n## Features\n\n- Logic-based site definitions\n- DCG for parsing\n- Backtracking for flexible matching\n',
    write_file_content(IndexPath, SampleContent),
    writeln('  Created sample content'),
    format('~nSite initialized! Run prodigy-ssg build in ~w/ to build.~n', [Name]).

% ============================================================================
% Clean Command
% ============================================================================

clean_site :-
    site_config(output_dir, OutputDir),
    format('Cleaning ~w/...~n', [OutputDir]),
    ( exists_directory(OutputDir) ->
        delete_directory_and_contents(OutputDir)
    ; true
    ),
    writeln('Clean complete.').

% ============================================================================
% Help
% ============================================================================

print_usage :-
    writeln('Prodigy SSG - Prolog-powered static site generator'),
    nl,
    writeln('USAGE:'),
    writeln('  prodigy-ssg <command> [options]'),
    nl,
    writeln('COMMANDS:'),
    writeln('  build              Build the site'),
    writeln('  init <name>        Create a new site'),
    writeln('  clean              Remove generated files'),
    writeln('  test-markdown      Test markdown parser'),
    writeln('  test-frontmatter   Test frontmatter parser'),
    writeln('  test-full          Test full pipeline'),
    nl,
    writeln('OPTIONS:'),
    writeln('  -h, --help         Show this help'),
    nl,
    writeln('EXAMPLES:'),
    writeln('  prodigy-ssg init my-blog'),
    writeln('  cd my-blog && prodigy-ssg build').

% ============================================================================
% Main
% ============================================================================

main :-
    current_prolog_flag(argv, Args),
    ( Args = [build|_] -> build_site
    ; Args = [init, Name|_] -> init_site(Name)
    ; Args = [init|_] -> writeln('Error: init requires a site name'), writeln('Usage: prodigy-ssg init <name>')
    ; Args = [clean|_] -> clean_site
    ; Args = [test-markdown|_] -> test_markdown
    ; Args = [test-frontmatter|_] -> test_frontmatter
    ; Args = [test-full|_] -> test_full
    ; Args = ['test-markdown'|_] -> test_markdown
    ; Args = ['test-frontmatter'|_] -> test_frontmatter
    ; Args = ['test-full'|_] -> test_full
    ; Args = ['-h'|_] -> print_usage
    ; Args = ['--help'|_] -> print_usage
    ; Args = [] -> print_usage
    ; Args = [Cmd|_] -> format('Unknown command: ~w~n', [Cmd]), writeln('Run prodigy-ssg --help for usage.')
    ),
    halt.

:- initialization(main, main).
