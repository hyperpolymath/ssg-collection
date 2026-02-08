%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% prodigy_frontmatter.pl - YAML Frontmatter Parser
%%
%% Handles parsing of YAML frontmatter in content files.
%% Supports common frontmatter fields: title, date, tags, draft, layout.

:- module(prodigy_frontmatter, [
    parse_frontmatter/3,
    extract_field/3,
    is_draft/1,
    get_title/2,
    get_date/2,
    get_tags/2,
    get_layout/2
]).

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(apply)).

%% parse_frontmatter(+Content, -Frontmatter, -Body)
%% Parse YAML frontmatter from content, returning frontmatter dict and body
parse_frontmatter(Content, Frontmatter, Body) :-
    string_codes(Content, Codes),
    phrase(frontmatter_block(FmPairs, BodyCodes), Codes),
    dict_create(Frontmatter, frontmatter, FmPairs),
    string_codes(Body, BodyCodes).

%% DCG for frontmatter block
frontmatter_block(Pairs, Body) -->
    "---", blanks, "\n",
    yaml_lines(Pairs),
    "---", blanks, optional_newline,
    remainder(Body).

%% Parse multiple YAML lines
yaml_lines([]) --> [].
yaml_lines([Pair|Pairs]) -->
    yaml_line(Pair),
    yaml_lines(Pairs).
yaml_lines(Pairs) -->
    blank_line,
    yaml_lines(Pairs).

%% Parse a single YAML key: value line
yaml_line(Key-Value) -->
    yaml_key(KeyAtom),
    ":",
    blanks,
    yaml_value(Value),
    "\n",
    { atom_string(Key, KeyAtom) }.

%% Parse YAML key (alphanumeric + underscore + hyphen)
yaml_key(Key) -->
    yaml_key_chars(Chars),
    { Chars \= [], atom_codes(Key, Chars) }.

yaml_key_chars([C|Cs]) -->
    [C],
    { code_type(C, alnum) ; C = 0'_ ; C = 0'- },
    yaml_key_chars(Cs).
yaml_key_chars([]) --> [].

%% Parse YAML value
yaml_value(Value) -->
    quoted_string(Value), !.
yaml_value(Value) -->
    yaml_list(Value), !.
yaml_value(Value) -->
    yaml_scalar(Value).

%% Quoted string value
quoted_string(Value) -->
    "\"",
    string_without("\"", Chars),
    "\"",
    { string_codes(Value, Chars) }.
quoted_string(Value) -->
    "'",
    string_without("'", Chars),
    "'",
    { string_codes(Value, Chars) }.

%% YAML inline list [item1, item2, ...]
yaml_list(Items) -->
    "[",
    blanks,
    yaml_list_items(Items),
    blanks,
    "]".

yaml_list_items([Item|Items]) -->
    yaml_list_item(Item),
    blanks,
    ( "," -> blanks, yaml_list_items(Items) ; { Items = [] } ).
yaml_list_items([]) --> [].

yaml_list_item(Item) -->
    quoted_string(Item), !.
yaml_list_item(Item) -->
    string_without(",]\n", Chars),
    { string_codes(ItemRaw, Chars),
      split_string(ItemRaw, "", " \t", [Item|_]) }.

%% Scalar value (unquoted)
yaml_scalar(Value) -->
    string_without("\n", Chars),
    { string_codes(ValueRaw, Chars),
      normalize_space(string(Value), ValueRaw) }.

%% Helper: string without certain characters
string_without(_, []) --> [].
string_without(End, [C|Cs]) -->
    [C],
    { \+ memberchk(C, End) },
    string_without(End, Cs).
string_without(End, []) -->
    { string_codes(EndStr, End), EndStr = EndStr }.

%% Optional newline
optional_newline --> "\n", !.
optional_newline --> [].

%% Blank line
blank_line --> blanks, "\n".

%% Remainder of input
remainder([]) --> [].
remainder([C|Cs]) --> [C], remainder(Cs).

%% extract_field(+Frontmatter, +Field, -Value)
%% Extract a field from frontmatter
extract_field(Fm, Field, Value) :-
    get_dict(Field, Fm, Value).

%% is_draft(+Frontmatter)
%% Check if content is marked as draft
is_draft(Fm) :-
    get_dict(draft, Fm, Draft),
    ( Draft = true ; Draft = "true" ).

%% get_title(+Frontmatter, -Title)
get_title(Fm, Title) :-
    ( get_dict(title, Fm, Title) -> true ; Title = "Untitled" ).

%% get_date(+Frontmatter, -Date)
get_date(Fm, Date) :-
    ( get_dict(date, Fm, Date) -> true ; Date = "" ).

%% get_tags(+Frontmatter, -Tags)
get_tags(Fm, Tags) :-
    ( get_dict(tags, Fm, Tags) -> true ; Tags = [] ).

%% get_layout(+Frontmatter, -Layout)
get_layout(Fm, Layout) :-
    ( get_dict(layout, Fm, Layout) -> true ; Layout = "default" ).
