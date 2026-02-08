"
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2025 hyperpolymath

PharosBuilder - Main entry point for building sites.
Provides both interactive and headless build modes.
"
Class {
	#name : #PharosBuilder,
	#superclass : #Object,
	#instVars : [
		'site',
		'contentDir',
		'outputDir'
	],
	#classVars : [
		'Current'
	],
	#category : #'Pharos-SSG-Builder'
}

{ #category : #'instance creation' }
PharosBuilder class >> forSite: aSite [
	^ self new
		site: aSite;
		yourself
]

{ #category : #accessing }
PharosBuilder class >> current [
	^ Current
]

{ #category : #accessing }
PharosBuilder class >> current: aBuilder [
	Current := aBuilder
]

{ #category : #initialization }
PharosBuilder >> initialize [
	super initialize.
	contentDir := 'content' asFileReference.
	outputDir := '_site' asFileReference
]

{ #category : #accessing }
PharosBuilder >> site [
	^ site
]

{ #category : #accessing }
PharosBuilder >> site: aSite [
	site := aSite
]

{ #category : #accessing }
PharosBuilder >> contentDir [
	^ contentDir
]

{ #category : #accessing }
PharosBuilder >> contentDir: aFileReference [
	contentDir := aFileReference asFileReference
]

{ #category : #accessing }
PharosBuilder >> outputDir [
	^ outputDir
]

{ #category : #accessing }
PharosBuilder >> outputDir: aFileReference [
	outputDir := aFileReference asFileReference
]

{ #category : #loading }
PharosBuilder >> loadContent [
	"Load all content files from the content directory"
	contentDir allFiles do: [ :file |
		(file extension = 'md') ifTrue: [
			self loadMarkdownFile: file
		]
	]
]

{ #category : #loading }
PharosBuilder >> loadMarkdownFile: aFileReference [
	"Load a single Markdown file as a page"
	| page content path frontMatter |
	content := aFileReference contents.
	path := '/' , (aFileReference relativeTo: contentDir) pathString.
	path := path copyReplaceAll: '.md' with: ''.

	"Parse front matter if present"
	(content beginsWith: '---') ifTrue: [
		| parts |
		parts := content splitOn: '---'.
		frontMatter := self parseFrontMatter: (parts at: 2).
		content := parts allButFirst: 2 joinUsing: '---'
	] ifFalse: [
		frontMatter := Dictionary new
	].

	page := PharosPage
		titled: (frontMatter at: 'title' ifAbsent: [ aFileReference basenameWithoutExtension ])
		path: path.
	page content: content.
	page metadata: frontMatter.

	site addPage: page.
	^ page
]

{ #category : #loading }
PharosBuilder >> parseFrontMatter: yamlString [
	"Parse YAML-like front matter into a Dictionary"
	| dict |
	dict := Dictionary new.
	yamlString lines do: [ :line |
		| parts |
		(line includes: $:) ifTrue: [
			parts := line splitOn: ':'.
			dict at: (parts first trimBoth) put: (parts allButFirst joinUsing: ':') trimBoth
		]
	].
	^ dict
]

{ #category : #building }
PharosBuilder >> build [
	"Full build: load content, process, and output"
	Transcript show: 'Pharos: Starting build...'; cr.

	self loadContent.
	Transcript show: 'Pharos: Loaded ', site pages size asString, ' pages'; cr.

	site build.
	Transcript show: 'Pharos: Build complete -> ', outputDir pathString; cr.

	^ outputDir
]

{ #category : #building }
PharosBuilder >> buildHeadless [
	"Build without any GUI - for CI/CD"
	^ self build
]

{ #category : #'interactive' }
PharosBuilder >> open [
	"Open the live editing environment"
	self class current: self.
	site inspect
]

{ #category : #'command line' }
PharosBuilder class >> runHeadless [
	"Entry point for headless builds"
	| builder site |
	site := PharosSite named: 'pharos-site'.
	builder := self forSite: site.
	builder contentDir: 'content'.
	builder outputDir: '_site'.
	builder buildHeadless.
	Smalltalk snapshot: false andQuit: true
]
