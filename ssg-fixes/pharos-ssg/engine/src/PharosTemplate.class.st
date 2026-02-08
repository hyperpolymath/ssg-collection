"
SPDX-License-Identifier: AGPL-3.0-or-later
SPDX-FileCopyrightText: 2025 hyperpolymath

PharosTemplate - A template for rendering pages.
Templates are Smalltalk blocks that receive a page
and produce HTML output.
"
Class {
	#name : #PharosTemplate,
	#superclass : #Object,
	#instVars : [
		'name',
		'renderBlock',
		'layout'
	],
	#category : #'Pharos-SSG-Core'
}

{ #category : #'instance creation' }
PharosTemplate class >> named: aString render: aBlock [
	^ self new
		name: aString;
		renderBlock: aBlock;
		yourself
]

{ #category : #accessing }
PharosTemplate >> name [
	^ name
]

{ #category : #accessing }
PharosTemplate >> name: aString [
	name := aString
]

{ #category : #accessing }
PharosTemplate >> renderBlock [
	^ renderBlock
]

{ #category : #accessing }
PharosTemplate >> renderBlock: aBlock [
	renderBlock := aBlock
]

{ #category : #accessing }
PharosTemplate >> layout [
	^ layout
]

{ #category : #accessing }
PharosTemplate >> layout: aTemplate [
	layout := aTemplate
]

{ #category : #rendering }
PharosTemplate >> renderPage: aPage [
	"Render a page using this template"
	| content html |
	content := renderBlock value: aPage.
	layout
		ifNil: [ html := content ]
		ifNotNil: [ html := layout wrapContent: content page: aPage ].
	^ html
]

{ #category : #rendering }
PharosTemplate >> wrapContent: contentHtml page: aPage [
	"Wrap content HTML in this template's layout"
	^ renderBlock value: aPage value: contentHtml
]
