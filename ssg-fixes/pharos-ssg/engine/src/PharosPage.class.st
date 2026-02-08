"
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2025 hyperpolymath

PharosPage - A single page in the site object graph.
Pages are live objects that can be inspected, modified,
and rendered at any time.
"
Class {
	#name : #PharosPage,
	#superclass : #Object,
	#instVars : [
		'title',
		'path',
		'content',
		'metadata',
		'template',
		'site',
		'children'
	],
	#category : #'Pharos-SSG-Core'
}

{ #category : #'instance creation' }
PharosPage class >> titled: aString path: aPath [
	^ self new
		title: aString;
		path: aPath;
		yourself
]

{ #category : #initialization }
PharosPage >> initialize [
	super initialize.
	metadata := Dictionary new.
	children := OrderedCollection new
]

{ #category : #accessing }
PharosPage >> title [
	^ title
]

{ #category : #accessing }
PharosPage >> title: aString [
	title := aString
]

{ #category : #accessing }
PharosPage >> path [
	^ path
]

{ #category : #accessing }
PharosPage >> path: aString [
	path := aString
]

{ #category : #accessing }
PharosPage >> content [
	^ content
]

{ #category : #accessing }
PharosPage >> content: aString [
	content := aString
]

{ #category : #accessing }
PharosPage >> template [
	^ template
]

{ #category : #accessing }
PharosPage >> template: aTemplateOrName [
	template := aTemplateOrName
]

{ #category : #accessing }
PharosPage >> site [
	^ site
]

{ #category : #accessing }
PharosPage >> site: aSite [
	site := aSite
]

{ #category : #accessing }
PharosPage >> metadata [
	^ metadata
]

{ #category : #accessing }
PharosPage >> children [
	^ children
]

{ #category : #'hierarchy' }
PharosPage >> addChild: aPage [
	children add: aPage.
	^ aPage
]

{ #category : #rendering }
PharosPage >> render [
	"Render this page to HTML using its template"
	| tmpl html |
	tmpl := self resolveTemplate.
	html := tmpl
		ifNil: [ self renderContent ]
		ifNotNil: [ tmpl renderPage: self ].
	^ html
]

{ #category : #rendering }
PharosPage >> renderContent [
	"Render just the content (without template wrapper)"
	^ PharosMarkdownParser new parse: content
]

{ #category : #rendering }
PharosPage >> resolveTemplate [
	"Find the template for this page"
	template ifNil: [ ^ nil ].
	template isString ifTrue: [
		^ site templateNamed: template
	].
	^ template
]

{ #category : #building }
PharosPage >> buildTo: outputDir [
	"Build this page and write to the output directory"
	| outPath html |
	outPath := outputDir / (self outputPath).
	outPath parent ensureCreateDirectory.
	html := self render.
	outPath writeStreamDo: [ :stream |
		stream nextPutAll: html
	].
	^ outPath
]

{ #category : #building }
PharosPage >> outputPath [
	"Convert source path to output path (e.g., /about -> /about/index.html)"
	path = '/' ifTrue: [ ^ 'index.html' ].
	(path endsWith: '.html') ifTrue: [ ^ path allButFirst ].
	^ path allButFirst , '/index.html'
]

{ #category : #navigation }
PharosPage >> links [
	"Extract all internal links from the content"
	| linkRegex matches |
	content ifNil: [ ^ #() ].
	linkRegex := '\[([^\]]+)\]\(([^)]+)\)' asRegex.
	matches := OrderedCollection new.
	content allRegexMatchesDo: [ :match |
		| href |
		href := (match at: 2).
		(href beginsWith: '/') ifTrue: [ matches add: href ]
	] regex: linkRegex.
	^ matches
]

{ #category : #inspection }
PharosPage >> gtInspectorContentIn: composite [
	<gtInspectorPresentationOrder: 1>
	^ composite text
		title: 'Content';
		display: [ content ifNil: [ '' ] ]
]

{ #category : #inspection }
PharosPage >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 2>
	^ composite morph
		title: 'Preview';
		display: [
			| html |
			html := self render.
			(WebBrowser openOnHtml: html) asMorph
		]
]
