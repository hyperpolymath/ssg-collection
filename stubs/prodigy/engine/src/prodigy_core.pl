#!/usr/bin/env swipl
%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% prodigy_core.pl - Core engine for Prodigy SSG
%%
%% This module provides the central coordination for the static site generator.
%% Prolog's logical inference drives the entire build process.

:- module(prodigy_core, [
    build_site/1,
    build_site/2,
    process_content/2,
    generate_output/2,
    site_config/1,
    version/1
]).

:- use_module(library(filesex)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(lists)).

%% Version information
version('1.1.0').

%% Default site configuration
:- dynamic site_config/1.
site_config(config{
    input_dir: 'content',
    output_dir: '_site',
    templates_dir: 'templates',
    base_url: '/',
    title: 'Prodigy Site'
}).

%% build_site(+OutputDir)
%% Build the entire site to the specified output directory
build_site(OutputDir) :-
    site_config(Config),
    build_site(Config, OutputDir).

%% build_site(+Config, +OutputDir)
%% Build site with explicit configuration
build_site(Config, OutputDir) :-
    format("~n=== Prodigy SSG Build ===~n"),
    format("Output: ~w~n", [OutputDir]),
    ensure_output_dir(OutputDir),
    InputDir = Config.input_dir,
    find_content_files(InputDir, Files),
    length(Files, Count),
    format("Processing ~w files...~n", [Count]),
    maplist(process_and_write(Config, OutputDir), Files),
    format("Build complete!~n~n").

%% ensure_output_dir(+Dir)
%% Create output directory if it doesn't exist
ensure_output_dir(Dir) :-
    ( exists_directory(Dir) -> true ; make_directory_path(Dir) ).

%% find_content_files(+Dir, -Files)
%% Find all markdown files in the content directory
find_content_files(Dir, Files) :-
    ( exists_directory(Dir)
    -> directory_files(Dir, AllFiles),
       include(is_markdown_file, AllFiles, Files)
    ;  Files = []
    ).

%% is_markdown_file(+File)
%% Check if file is a markdown file
is_markdown_file(File) :-
    ( sub_atom(File, _, _, 0, '.md')
    ; sub_atom(File, _, _, 0, '.markdown')
    ).

%% process_and_write(+Config, +OutputDir, +File)
%% Process a content file and write output
process_and_write(Config, OutputDir, File) :-
    InputDir = Config.input_dir,
    atomic_list_concat([InputDir, '/', File], InputPath),
    process_content(InputPath, Result),
    output_filename(File, OutputFile),
    atomic_list_concat([OutputDir, '/', OutputFile], OutputPath),
    generate_output(Result, OutputPath).

%% output_filename(+InputFile, -OutputFile)
%% Convert input filename to output filename
output_filename(InputFile, OutputFile) :-
    ( sub_atom(InputFile, Before, _, _, '.md')
    -> sub_atom(InputFile, 0, Before, _, Base),
       atom_concat(Base, '.html', OutputFile)
    ; sub_atom(InputFile, Before, _, _, '.markdown')
    -> sub_atom(InputFile, 0, Before, _, Base),
       atom_concat(Base, '.html', OutputFile)
    ; atom_concat(InputFile, '.html', OutputFile)
    ).

%% process_content(+FilePath, -Result)
%% Process a content file and return structured result
process_content(FilePath, Result) :-
    ( exists_file(FilePath)
    -> read_file_to_string(FilePath, Content, []),
       parse_content(Content, Result)
    ;  Result = error{message: 'File not found', path: FilePath}
    ).

%% parse_content(+Content, -Result)
%% Parse raw content into structured data
parse_content(Content, Result) :-
    ( parse_frontmatter(Content, Frontmatter, Body)
    -> parse_markdown(Body, Html),
       Result = page{frontmatter: Frontmatter, body: Html, raw: Content}
    ;  parse_markdown(Content, Html),
       Result = page{frontmatter: _{}, body: Html, raw: Content}
    ).

%% parse_frontmatter(+Content, -Frontmatter, -Body)
%% Extract YAML frontmatter from content
parse_frontmatter(Content, Frontmatter, Body) :-
    sub_string(Content, 0, 3, _, "---"),
    sub_string(Content, 3, _, _, Rest),
    sub_string(Rest, Before, 3, After, "---"),
    sub_string(Rest, 0, Before, _, FmRaw),
    sub_string(Rest, _, After, 0, BodyRaw),
    string_codes(BodyRaw, [_|BodyCodes]),  % Skip newline
    string_codes(Body, BodyCodes),
    parse_yaml_simple(FmRaw, Frontmatter).

%% parse_yaml_simple(+YamlString, -Dict)
%% Simple YAML key-value parser
parse_yaml_simple(Yaml, Dict) :-
    split_string(Yaml, "\n", "\r\t ", Lines),
    include([L]>>(\+ L = ""), Lines, NonEmpty),
    maplist(parse_yaml_line, NonEmpty, Pairs),
    dict_create(Dict, yaml, Pairs).

%% parse_yaml_line(+Line, -Key-Value)
%% Parse a single YAML line
parse_yaml_line(Line, Key-Value) :-
    sub_string(Line, Before, 1, After, ":"),
    sub_string(Line, 0, Before, _, KeyStr),
    sub_string(Line, _, After, 0, ValueRaw),
    string_codes(KeyStr, KeyCodes),
    atom_codes(Key, KeyCodes),
    split_string(ValueRaw, "", " \t\"'", [Value|_]).

%% parse_markdown(+Markdown, -Html)
%% Convert markdown to HTML (delegate to markdown module)
parse_markdown(Markdown, Html) :-
    % Simple inline conversion for now
    string_codes(Markdown, Codes),
    phrase(markdown_to_html(HtmlCodes), Codes),
    string_codes(Html, HtmlCodes).

%% DCG for markdown to HTML conversion
markdown_to_html([]) --> [].
markdown_to_html(Html) -->
    md_element(Elem),
    markdown_to_html(Rest),
    { append(Elem, Rest, Html) }.

%% Markdown elements
md_element(Html) --> md_header(Html).
md_element(Html) --> md_list_item(Html).
md_element(Html) --> md_paragraph(Html).
md_element([C]) --> [C].

%% Headers
md_header(Html) -->
    "# ", string_without("\n", Title), "\n",
    { append("<h1>", Title, T1), append(T1, "</h1>\n", Html) }.
md_header(Html) -->
    "## ", string_without("\n", Title), "\n",
    { append("<h2>", Title, T1), append(T1, "</h2>\n", Html) }.
md_header(Html) -->
    "### ", string_without("\n", Title), "\n",
    { append("<h3>", Title, T1), append(T1, "</h3>\n", Html) }.

%% List items
md_list_item(Html) -->
    "- ", string_without("\n", Item), "\n",
    { append("<li>", Item, T1), append(T1, "</li>\n", Html) }.
md_list_item(Html) -->
    "* ", string_without("\n", Item), "\n",
    { append("<li>", Item, T1), append(T1, "</li>\n", Html) }.

%% Paragraphs
md_paragraph(Html) -->
    string_without("\n", Text), "\n",
    { Text \= [],
      append("<p>", Text, T1), append(T1, "</p>\n", Html) }.

%% Helper for string matching
string_without(_, []) --> [].
string_without(End, [C|Cs]) -->
    [C],
    { \+ memberchk(C, End) },
    string_without(End, Cs).
string_without(End, []) -->
    { End = End }.  % Lookahead

%% generate_output(+Result, +OutputPath)
%% Write the processed result to a file
generate_output(Result, OutputPath) :-
    ( is_dict(Result), get_dict(body, Result, Body)
    -> apply_template(Result, FinalHtml),
       write_output_file(OutputPath, FinalHtml)
    ;  format("Error: Invalid result for ~w~n", [OutputPath])
    ).

%% apply_template(+Page, -Html)
%% Apply HTML template to page content
apply_template(Page, Html) :-
    get_dict(frontmatter, Page, Fm),
    get_dict(body, Page, Body),
    ( get_dict(title, Fm, Title) -> true ; Title = "Untitled" ),
    format(atom(Html),
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>~w</title>
<style>
body { font-family: system-ui, -apple-system, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }
pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }
code { background: #f4f4f4; padding: 0.2em 0.4em; }
</style>
</head>
<body>
<article>
~w
</article>
</body>
</html>', [Title, Body]).

%% write_output_file(+Path, +Content)
%% Write content to file
write_output_file(Path, Content) :-
    file_directory_name(Path, Dir),
    ensure_output_dir(Dir),
    open(Path, write, Stream),
    write(Stream, Content),
    close(Stream),
    format("  Generated: ~w~n", [Path]).
