"
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2025 hyperpolymath

PharosMarkdownParser - Minimal Markdown to HTML parser.
Implements the subset of Markdown needed for static sites.
"
Class {
	#name : #PharosMarkdownParser,
	#superclass : #Object,
	#category : #'Pharos-SSG-Parser'
}

{ #category : #parsing }
PharosMarkdownParser >> parse: markdownString [
	"Convert Markdown to HTML"
	| html lines |
	markdownString ifNil: [ ^ '' ].
	lines := markdownString lines.
	html := String streamContents: [ :stream |
		self parseLines: lines to: stream
	].
	^ html
]

{ #category : #parsing }
PharosMarkdownParser >> parseLines: lines to: stream [
	"Process lines of Markdown"
	| inCodeBlock inParagraph |
	inCodeBlock := false.
	inParagraph := false.

	lines do: [ :line |
		| trimmed |
		trimmed := line trimBoth.

		"Code blocks"
		(trimmed beginsWith: '```') ifTrue: [
			inCodeBlock
				ifTrue: [
					stream nextPutAll: '</code></pre>'.
					inCodeBlock := false ]
				ifFalse: [
					stream nextPutAll: '<pre><code>'.
					inCodeBlock := true ].
			^ self
		].

		inCodeBlock ifTrue: [
			stream nextPutAll: (self escapeHtml: line); cr.
			^ self
		].

		"Empty line ends paragraph"
		trimmed isEmpty ifTrue: [
			inParagraph ifTrue: [
				stream nextPutAll: '</p>'.
				inParagraph := false
			].
			^ self
		].

		"Headers"
		(trimmed beginsWith: '# ') ifTrue: [
			stream nextPutAll: '<h1>'; nextPutAll: (trimmed allButFirst: 2); nextPutAll: '</h1>'.
			^ self
		].
		(trimmed beginsWith: '## ') ifTrue: [
			stream nextPutAll: '<h2>'; nextPutAll: (trimmed allButFirst: 3); nextPutAll: '</h2>'.
			^ self
		].
		(trimmed beginsWith: '### ') ifTrue: [
			stream nextPutAll: '<h3>'; nextPutAll: (trimmed allButFirst: 4); nextPutAll: '</h3>'.
			^ self
		].

		"List items"
		((trimmed beginsWith: '* ') or: [ trimmed beginsWith: '- ' ]) ifTrue: [
			stream nextPutAll: '<li>'; nextPutAll: (self parseInline: (trimmed allButFirst: 2)); nextPutAll: '</li>'.
			^ self
		].

		"Paragraph"
		inParagraph ifFalse: [
			stream nextPutAll: '<p>'.
			inParagraph := true
		] ifTrue: [
			stream space
		].
		stream nextPutAll: (self parseInline: trimmed)
	].

	inParagraph ifTrue: [ stream nextPutAll: '</p>' ]
]

{ #category : #parsing }
PharosMarkdownParser >> parseInline: text [
	"Parse inline Markdown elements"
	| result |
	result := text.

	"Bold **text**"
	result := result copyReplaceAll: '**' with: '<strong>' startingAt: 1.
	result := result copyReplaceAll: '</strong>' with: '</strong>' startingAt: 1.

	"Links [text](url)"
	result := self parseLinks: result.

	"Code `text`"
	result := result copyReplaceAll: '`' with: '<code>' startingAt: 1.

	^ result
]

{ #category : #parsing }
PharosMarkdownParser >> parseLinks: text [
	"Convert [text](url) to <a href='url'>text</a>"
	| regex |
	regex := '\[([^\]]+)\]\(([^)]+)\)' asRegex.
	^ text copyWithRegex: regex matchesReplacedWith: [ :match |
		'<a href="' , (match at: 2) , '">' , (match at: 1) , '</a>'
	]
]

{ #category : #private }
PharosMarkdownParser >> escapeHtml: text [
	"Escape HTML special characters"
	^ text
		copyReplaceAll: '&' with: '&amp;';
		copyReplaceAll: '<' with: '&lt;';
		copyReplaceAll: '>' with: '&gt;'
]
