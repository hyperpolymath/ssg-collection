---
title: <script>alert('XSS')</script>
date: 2024-01-01
author: <img src=x onerror=alert('XSS')>
---

# HTML Injection Tests

<script>alert('XSS in content')</script>

<img src="x" onerror="alert('XSS')">

<iframe src="javascript:alert('XSS')"></iframe>

<svg onload="alert('XSS')">

<body onload="alert('XSS')">

<div style="background:url(javascript:alert('XSS'))">

<a href="javascript:alert('XSS')">Click me</a>

<form action="javascript:alert('XSS')"><input type="submit"></form>

## Event Handlers

<div onclick="alert('XSS')">Click</div>
<div onmouseover="alert('XSS')">Hover</div>
<div onfocus="alert('XSS')" tabindex="1">Focus</div>

## Data URIs

<a href="data:text/html,<script>alert('XSS')</script>">Link</a>
<img src="data:image/svg+xml,<svg onload=alert('XSS')>">

## CSS Injection

<style>body { background: url("javascript:alert('XSS')") }</style>
<div style="expression(alert('XSS'))">IE specific</div>

## Comment Tricks

<!--<script>alert('XSS')</script>-->
<!-- --><script>alert('XSS')</script><!-- -->
