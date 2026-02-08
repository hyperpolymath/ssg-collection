"
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2025 hyperpolymath

PharosSite - The root object representing an entire static site.
A site is a live object graph where pages, templates, and assets
exist as interconnected Smalltalk objects.
"
Class {
	#name : #PharosSite,
	#superclass : #Object,
	#instVars : [
		'name',
		'baseUrl',
		'pages',
		'templates',
		'assets',
		'config'
	],
	#category : #'Pharos-SSG-Core'
}

{ #category : #'instance creation' }
PharosSite class >> named: aString [
	^ self new
		name: aString;
		yourself
]

{ #category : #initialization }
PharosSite >> initialize [
	super initialize.
	pages := OrderedCollection new.
	templates := Dictionary new.
	assets := OrderedCollection new.
	config := Dictionary new
]

{ #category : #accessing }
PharosSite >> name [
	^ name
]

{ #category : #accessing }
PharosSite >> name: aString [
	name := aString
]

{ #category : #accessing }
PharosSite >> baseUrl [
	^ baseUrl
]

{ #category : #accessing }
PharosSite >> baseUrl: aString [
	baseUrl := aString
]

{ #category : #accessing }
PharosSite >> pages [
	^ pages
]

{ #category : #accessing }
PharosSite >> templates [
	^ templates
]

{ #category : #accessing }
PharosSite >> assets [
	^ assets
]

{ #category : #'page management' }
PharosSite >> addPage: aPage [
	pages add: aPage.
	aPage site: self.
	^ aPage
]

{ #category : #'page management' }
PharosSite >> pageAt: aPath [
	"Find a page by its path"
	^ pages detect: [ :p | p path = aPath ] ifNone: [ nil ]
]

{ #category : #'template management' }
PharosSite >> addTemplate: aTemplate named: aName [
	templates at: aName put: aTemplate.
	^ aTemplate
]

{ #category : #'template management' }
PharosSite >> templateNamed: aName [
	^ templates at: aName ifAbsent: [ nil ]
]

{ #category : #building }
PharosSite >> build [
	"Build all pages and emit HTML to the output directory"
	| outputDir |
	outputDir := (config at: #outputDir ifAbsent: [ '_site' ]) asFileReference.
	outputDir ensureCreateDirectory.

	pages do: [ :page |
		page buildTo: outputDir
	].

	assets do: [ :asset |
		asset copyTo: outputDir
	].

	^ outputDir
]

{ #category : #building }
PharosSite >> buildHeadless [
	"Build without GUI - for CI/CD pipelines"
	^ self build
]

{ #category : #navigation }
PharosSite >> allLinks [
	"Collect all internal links across pages"
	^ pages flatCollect: [ :p | p links ]
]

{ #category : #navigation }
PharosSite >> brokenLinks [
	"Find links pointing to non-existent pages"
	| allPaths |
	allPaths := pages collect: [ :p | p path ].
	^ self allLinks reject: [ :link |
		(link beginsWith: 'http') or: [ allPaths includes: link ]
	]
]

{ #category : #inspection }
PharosSite >> gtInspectorPagesIn: composite [
	<gtInspectorPresentationOrder: 1>
	^ composite list
		title: 'Pages';
		display: [ pages ];
		format: [ :page | page title , ' (' , page path , ')' ]
]
