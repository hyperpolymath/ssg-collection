<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
---
title: Invalid YAML
date: [not: valid: yaml: here
tags: {broken
nested:
  - incomplete
  array: value
  : missing key
---

# Content After Bad YAML

This should be handled gracefully.
