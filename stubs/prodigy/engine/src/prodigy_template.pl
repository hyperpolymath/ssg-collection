%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% prodigy_template.pl - Template Engine
%%
%% Mustache-style variable substitution with Prolog pattern matching.
%% Supports: {{ variable }}, {{# section }}...{{/ section }}, {{^ inverted }}

:- module(prodigy_template, [
    apply_template/3,
    load_template/2,
    render/3,
    register_helper/2
]).

:- use_module(library(pcre)).
:- use_module(library(lists)).
:- use_module(library(apply)).

%% Dynamic helper registration
:- dynamic registered_helper/2.

%% apply_template(+TemplateName, +Context, -Output)
%% Apply a named template with context data
apply_template(TemplateName, Context, Output) :-
    load_template(TemplateName, Template),
    render(Template, Context, Output).

%% load_template(+Name, -Template)
%% Load a template from the templates directory
load_template(Name, Template) :-
    atomic_list_concat(['templates/', Name, '.html'], Path),
    ( exists_file(Path)
    -> read_file_to_string(Path, Template, [])
    ;  default_template(Name, Template)
    ).

%% default_template(+Name, -Template)
%% Built-in default templates
default_template(page,
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{ title }}</title>
<style>
body { font-family: system-ui, -apple-system, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; color: #333; }
h1, h2, h3 { color: #111; }
pre { background: #f5f5f5; padding: 1rem; border-radius: 4px; overflow-x: auto; }
code { background: #f5f5f5; padding: 0.2em 0.4em; border-radius: 3px; font-size: 0.9em; }
a { color: #0066cc; text-decoration: none; }
a:hover { text-decoration: underline; }
.meta { color: #666; font-size: 0.9em; margin-bottom: 2rem; }
</style>
</head>
<body>
<article>
<h1>{{ title }}</h1>
{{# date }}<p class="meta">{{ date }}</p>{{/ date }}
{{{ content }}}
</article>
</body>
</html>').

default_template(index,
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{ site_title }}</title>
</head>
<body>
<header>
<h1>{{ site_title }}</h1>
</header>
<main>
{{# pages }}
<article>
<h2><a href="{{ url }}">{{ title }}</a></h2>
{{# date }}<time>{{ date }}</time>{{/ date }}
</article>
{{/ pages }}
</main>
</body>
</html>').

default_template(_, Template) :-
    default_template(page, Template).

%% render(+Template, +Context, -Output)
%% Render a template with context substitution
render(Template, Context, Output) :-
    string_codes(Template, Codes),
    phrase(template_content(Context, OutputCodes), Codes),
    string_codes(Output, OutputCodes).

%% DCG for template content
template_content(_, []) --> [].
template_content(Ctx, Output) -->
    template_element(Ctx, Elem),
    template_content(Ctx, Rest),
    { append(Elem, Rest, Output) }.

%% Template elements
template_element(Ctx, Output) --> section(Ctx, Output), !.
template_element(Ctx, Output) --> inverted_section(Ctx, Output), !.
template_element(Ctx, Output) --> unescaped_var(Ctx, Output), !.
template_element(Ctx, Output) --> escaped_var(Ctx, Output), !.
template_element(_, [C]) --> [C].

%% Escaped variable {{ var }}
escaped_var(Ctx, Output) -->
    "{{", whites, var_name(Name), whites, "}}",
    { lookup_var(Ctx, Name, Value),
      escape_html(Value, Escaped),
      string_codes(Escaped, Output) }.

%% Unescaped variable {{{ var }}}
unescaped_var(Ctx, Output) -->
    "{{{", whites, var_name(Name), whites, "}}}",
    { lookup_var(Ctx, Name, Value),
      ( string(Value) -> string_codes(Value, Output) ; atom_codes(Value, Output) ) }.

%% Section {{# name }}...{{/ name }}
section(Ctx, Output) -->
    "{{#", whites, var_name(Name), whites, "}}",
    section_content(Name, Content),
    "{{/", whites, var_name(Name), whites, "}}",
    { render_section(Ctx, Name, Content, Output) }.

%% Inverted section {{^ name }}...{{/ name }}
inverted_section(Ctx, Output) -->
    "{{^", whites, var_name(Name), whites, "}}",
    section_content(Name, Content),
    "{{/", whites, var_name(Name), whites, "}}",
    { render_inverted_section(Ctx, Name, Content, Output) }.

%% Section content (everything until closing tag)
section_content(Name, Content) -->
    section_content_chars(Name, Content).

section_content_chars(Name, []) -->
    { atom_codes(Name, NameCodes) },
    "{{/", whites, NameCodes, !.
section_content_chars(Name, [C|Cs]) -->
    [C],
    section_content_chars(Name, Cs).

%% Variable name
var_name(Name) -->
    var_chars(Chars),
    { Chars \= [], atom_codes(Name, Chars) }.

var_chars([C|Cs]) -->
    [C],
    { code_type(C, alnum) ; C = 0'_ ; C = 0'. },
    var_chars(Cs).
var_chars([]) --> [].

%% Whitespace
whites --> [C], { code_type(C, space) }, whites.
whites --> [].

%% lookup_var(+Context, +Name, -Value)
%% Look up a variable in the context
lookup_var(Ctx, Name, Value) :-
    ( is_dict(Ctx)
    -> ( get_dict(Name, Ctx, Value) -> true ; Value = "" )
    ; member(Name-Value, Ctx) -> true ; Value = ""
    ).

%% render_section(+Ctx, +Name, +Content, -Output)
%% Render a section based on its value
render_section(Ctx, Name, Content, Output) :-
    lookup_var(Ctx, Name, Value),
    ( Value = false -> Output = []
    ; Value = "" -> Output = []
    ; Value = [] -> Output = []
    ; is_list(Value) ->
        string_codes(ContentStr, Content),
        maplist({ContentStr}/[Item, Out]>>render(ContentStr, Item, Out), Value, Outs),
        atomics_to_string(Outs, OutputStr),
        string_codes(OutputStr, Output)
    ; string_codes(ContentStr, Content),
      render(ContentStr, Ctx, OutputStr),
      string_codes(OutputStr, Output)
    ).

%% render_inverted_section(+Ctx, +Name, +Content, -Output)
%% Render inverted section (shown when value is falsy)
render_inverted_section(Ctx, Name, Content, Output) :-
    lookup_var(Ctx, Name, Value),
    ( ( Value = false ; Value = "" ; Value = [] )
    -> string_codes(ContentStr, Content),
       render(ContentStr, Ctx, OutputStr),
       string_codes(OutputStr, Output)
    ; Output = []
    ).

%% escape_html(+Input, -Output)
%% Escape HTML special characters
escape_html(Input, Output) :-
    ( string(Input) -> S = Input ; atom_string(Input, S) ),
    string_codes(S, Codes),
    escape_codes(Codes, EscapedCodes),
    string_codes(Output, EscapedCodes).

escape_codes([], []).
escape_codes([0'<|Cs], Out) :- !,
    append("&lt;", Rest, Out),
    escape_codes(Cs, Rest).
escape_codes([0'>|Cs], Out) :- !,
    append("&gt;", Rest, Out),
    escape_codes(Cs, Rest).
escape_codes([0'&|Cs], Out) :- !,
    append("&amp;", Rest, Out),
    escape_codes(Cs, Rest).
escape_codes([0'"|Cs], Out) :- !,
    append("&quot;", Rest, Out),
    escape_codes(Cs, Rest).
escape_codes([C|Cs], [C|Rest]) :-
    escape_codes(Cs, Rest).

%% register_helper(+Name, +Predicate)
%% Register a template helper function
register_helper(Name, Pred) :-
    retractall(registered_helper(Name, _)),
    assertz(registered_helper(Name, Pred)).
