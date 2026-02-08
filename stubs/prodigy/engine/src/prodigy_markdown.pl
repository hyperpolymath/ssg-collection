%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
%%
%% prodigy_markdown.pl - Markdown Parser
%%
%% DCG-based markdown parser leveraging Prolog's grammar capabilities.
%% Supports: headers, lists, code blocks, inline formatting, links, images.

:- module(prodigy_markdown, [
    markdown_to_html/2,
    parse_inline/2,
    parse_block/2
]).

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(apply)).

%% markdown_to_html(+Markdown, -Html)
%% Convert markdown string to HTML string
markdown_to_html(Markdown, Html) :-
    string_codes(Markdown, Codes),
    phrase(markdown_document(Blocks), Codes),
    maplist(block_to_html, Blocks, HtmlParts),
    atomics_to_string(HtmlParts, Html).

%% DCG: Parse entire markdown document
markdown_document([]) --> eos, !.
markdown_document([Block|Blocks]) -->
    markdown_block(Block),
    markdown_document(Blocks).

%% DCG: Parse a single block element
markdown_block(Block) -->
    blank_lines,
    ( code_block(Block), !
    ; header(Block), !
    ; horizontal_rule(Block), !
    ; blockquote(Block), !
    ; unordered_list(Block), !
    ; ordered_list(Block), !
    ; paragraph(Block)
    ).

%% Blank lines (skip)
blank_lines --> "\n", !, blank_lines.
blank_lines --> [].

%% Headers: # H1, ## H2, ### H3, etc.
header(h1(Content)) --> "# ", line_content(Content), eol_or_eos.
header(h2(Content)) --> "## ", line_content(Content), eol_or_eos.
header(h3(Content)) --> "### ", line_content(Content), eol_or_eos.
header(h4(Content)) --> "#### ", line_content(Content), eol_or_eos.
header(h5(Content)) --> "##### ", line_content(Content), eol_or_eos.
header(h6(Content)) --> "###### ", line_content(Content), eol_or_eos.

%% Code blocks: ```lang ... ```
code_block(code_block(Lang, Code)) -->
    "```", optional_lang(Lang), "\n",
    code_content(CodeChars),
    "```", eol_or_eos,
    { string_codes(Code, CodeChars) }.

optional_lang(Lang) -->
    nonblanks(Chars),
    { Chars \= [], string_codes(Lang, Chars) }, !.
optional_lang("") --> [].

code_content([]) --> "```", !.
code_content([C|Cs]) --> [C], code_content(Cs).

%% Horizontal rule: ---, ***, ___
horizontal_rule(hr) --> "---", optional_dashes, eol_or_eos.
horizontal_rule(hr) --> "***", optional_stars, eol_or_eos.
horizontal_rule(hr) --> "___", optional_underscores, eol_or_eos.

optional_dashes --> "-", !, optional_dashes.
optional_dashes --> [].
optional_stars --> "*", !, optional_stars.
optional_stars --> [].
optional_underscores --> "_", !, optional_underscores.
optional_underscores --> [].

%% Blockquote: > text
blockquote(blockquote(Content)) -->
    "> ", line_content(Content), eol_or_eos.

%% Unordered list: - item, * item
unordered_list(ul(Items)) -->
    ul_item(First),
    ul_items(Rest),
    { Items = [First|Rest] }.

ul_item(li(Content)) --> "- ", line_content(Content), eol_or_eos.
ul_item(li(Content)) --> "* ", line_content(Content), eol_or_eos.

ul_items([Item|Items]) --> ul_item(Item), !, ul_items(Items).
ul_items([]) --> [].

%% Ordered list: 1. item
ordered_list(ol(Items)) -->
    ol_item(First),
    ol_items(Rest),
    { Items = [First|Rest] }.

ol_item(li(Content)) --> digit(_), ".", " ", line_content(Content), eol_or_eos.
ol_item(li(Content)) --> digit(_), digit(_), ".", " ", line_content(Content), eol_or_eos.

ol_items([Item|Items]) --> ol_item(Item), !, ol_items(Items).
ol_items([]) --> [].

%% Paragraph
paragraph(p(Content)) -->
    line_content(Content),
    { Content \= "" },
    eol_or_eos.

%% Line content (until newline)
line_content(Content) -->
    line_chars(Chars),
    { string_codes(Content, Chars) }.

line_chars([]) --> ( "\n" ; eos ), !.
line_chars([C|Cs]) --> [C], line_chars(Cs).

%% End of line or stream
eol_or_eos --> "\n", !.
eol_or_eos --> eos.

%% End of stream
eos([], []).

%% block_to_html(+Block, -Html)
%% Convert block element to HTML string
block_to_html(h1(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h1>~w</h1>~n", [Inline]).
block_to_html(h2(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h2>~w</h2>~n", [Inline]).
block_to_html(h3(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h3>~w</h3>~n", [Inline]).
block_to_html(h4(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h4>~w</h4>~n", [Inline]).
block_to_html(h5(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h5>~w</h5>~n", [Inline]).
block_to_html(h6(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<h6>~w</h6>~n", [Inline]).

block_to_html(p(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<p>~w</p>~n", [Inline]).

block_to_html(blockquote(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "<blockquote>~w</blockquote>~n", [Inline]).

block_to_html(hr, "<hr>\n").

block_to_html(code_block(Lang, Code), Html) :-
    escape_html(Code, Escaped),
    ( Lang = ""
    -> format(string(Html), "<pre><code>~w</code></pre>~n", [Escaped])
    ;  format(string(Html), "<pre><code class=\"language-~w\">~w</code></pre>~n", [Lang, Escaped])
    ).

block_to_html(ul(Items), Html) :-
    maplist(item_to_html, Items, ItemsHtml),
    atomics_to_string(ItemsHtml, ItemsStr),
    format(string(Html), "<ul>~n~w</ul>~n", [ItemsStr]).

block_to_html(ol(Items), Html) :-
    maplist(item_to_html, Items, ItemsHtml),
    atomics_to_string(ItemsHtml, ItemsStr),
    format(string(Html), "<ol>~n~w</ol>~n", [ItemsStr]).

item_to_html(li(Content), Html) :-
    parse_inline(Content, Inline),
    format(string(Html), "  <li>~w</li>~n", [Inline]).

%% parse_inline(+Text, -Html)
%% Parse inline formatting: bold, italic, code, links, images
parse_inline(Text, Html) :-
    string_codes(Text, Codes),
    phrase(inline_elements(Elements), Codes),
    maplist(inline_to_string, Elements, Parts),
    atomics_to_string(Parts, Html).

inline_elements([]) --> [].
inline_elements([Elem|Elems]) -->
    inline_element(Elem),
    inline_elements(Elems).

%% Inline elements
inline_element(Elem) --> inline_code(Elem), !.
inline_element(Elem) --> bold(Elem), !.
inline_element(Elem) --> italic(Elem), !.
inline_element(Elem) --> image(Elem), !.
inline_element(Elem) --> link(Elem), !.
inline_element(text([C])) --> [C].

%% Inline code: `code`
inline_code(code(Content)) -->
    "`", code_chars(Chars), "`",
    { string_codes(Content, Chars) }.

code_chars([]) --> "`", !.
code_chars([C|Cs]) --> [C], { C \= 0'` }, code_chars(Cs).

%% Bold: **text** or __text__
bold(bold(Content)) -->
    "**", inline_until("**", Content).
bold(bold(Content)) -->
    "__", inline_until("__", Content).

%% Italic: *text* or _text_
italic(italic(Content)) -->
    "*", { \+ peek("*") }, inline_until_single("*", Content).
italic(italic(Content)) -->
    "_", { \+ peek("_") }, inline_until_single("_", Content).

%% Helper for nested inline parsing
inline_until(End, Content) -->
    { string_codes(End, EndCodes) },
    chars_until(EndCodes, Chars),
    EndCodes,
    { string_codes(Content, Chars) }.

inline_until_single(End, Content) -->
    { string_codes(End, [EndCode]) },
    chars_until_single(EndCode, Chars),
    [EndCode],
    { string_codes(Content, Chars) }.

chars_until(End, []) --> End, !.
chars_until(End, [C|Cs]) --> [C], chars_until(End, Cs).

chars_until_single(End, []) --> [End], !.
chars_until_single(End, [C|Cs]) --> [C], { C \= End }, chars_until_single(End, Cs).

%% Peek without consuming
peek(S, In, In) :- string_codes(S, Codes), append(Codes, _, In).

%% Image: ![alt](url)
image(image(Alt, Url)) -->
    "![", chars_until("]", AltCodes), "]",
    "(", chars_until(")", UrlCodes), ")",
    { string_codes(Alt, AltCodes), string_codes(Url, UrlCodes) }.

%% Link: [text](url)
link(link(Text, Url)) -->
    "[", chars_until("]", TextCodes), "]",
    "(", chars_until(")", UrlCodes), ")",
    { string_codes(Text, TextCodes), string_codes(Url, UrlCodes) }.

%% inline_to_string(+Element, -String)
inline_to_string(text(Codes), S) :- string_codes(S, Codes).
inline_to_string(code(Content), S) :-
    escape_html(Content, Escaped),
    format(string(S), "<code>~w</code>", [Escaped]).
inline_to_string(bold(Content), S) :-
    parse_inline(Content, Inline),
    format(string(S), "<strong>~w</strong>", [Inline]).
inline_to_string(italic(Content), S) :-
    parse_inline(Content, Inline),
    format(string(S), "<em>~w</em>", [Inline]).
inline_to_string(link(Text, Url), S) :-
    format(string(S), "<a href=\"~w\">~w</a>", [Url, Text]).
inline_to_string(image(Alt, Url), S) :-
    format(string(S), "<img src=\"~w\" alt=\"~w\">", [Url, Alt]).

%% parse_block(+Text, -Blocks)
%% Parse text into block elements
parse_block(Text, Blocks) :-
    string_codes(Text, Codes),
    phrase(markdown_document(Blocks), Codes).

%% escape_html(+Input, -Output)
escape_html(Input, Output) :-
    string_codes(Input, Codes),
    escape_codes(Codes, EscapedCodes),
    string_codes(Output, EscapedCodes).

escape_codes([], []).
escape_codes([0'<|Cs], Out) :- append("&lt;", Rest, Out), escape_codes(Cs, Rest).
escape_codes([0'>|Cs], Out) :- append("&gt;", Rest, Out), escape_codes(Cs, Rest).
escape_codes([0'&|Cs], Out) :- append("&amp;", Rest, Out), escape_codes(Cs, Rest).
escape_codes([0'"|Cs], Out) :- append("&quot;", Rest, Out), escape_codes(Cs, Rest).
escape_codes([C|Cs], [C|Rest]) :- escape_codes(Cs, Rest).
