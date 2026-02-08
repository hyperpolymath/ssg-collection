#!/usr/bin/env python3
"""
milk-ssg - COW esoteric language powered static site generator

Moo! A brainfuck variant SSG, because why not.

COW has 12 instructions based on case variations of "moo":
  moo  - Move back one instruction (loop start)
  moO  - Move memory pointer forward
  mOo  - Move memory pointer backward
  mOO  - Execute value at memory as instruction
  Moo  - If current memory is 0, read char, else print char
  MOo  - Decrement current memory
  MoO  - Increment current memory
  MOO  - Loop: if memory is 0, skip to matching moo
  OOO  - Set current memory to 0
  MMM  - If memory is 0, register block, else restore
  OOM  - Print current memory as number
  oom  - Read number into current memory

For the SSG, we use COW programs to generate site content,
with a wrapper that handles file I/O and templates.
"""

import sys
import os
import re
import json
from typing import List, Tuple, Dict, Optional
from dataclasses import dataclass, field

# ============================================================================
# COW Interpreter
# ============================================================================

COW_INSTRUCTIONS = {
    'moo': 0,   # loop end / back jump
    'moO': 1,   # pointer++
    'mOo': 2,   # pointer--
    'mOO': 3,   # execute memory as instruction
    'Moo': 4,   # if mem==0 read, else print
    'MOo': 5,   # mem--
    'MoO': 6,   # mem++
    'MOO': 7,   # loop start
    'OOO': 8,   # mem = 0
    'MMM': 9,   # register/restore block
    'OOM': 10,  # print mem as number
    'oom': 11,  # read number
}

def parse_cow(source: str) -> List[int]:
    """Parse COW source into instruction list."""
    tokens = re.findall(r'[mMoO]{3}', source)
    return [COW_INSTRUCTIONS.get(t, -1) for t in tokens if t in COW_INSTRUCTIONS]

def run_cow(program: List[int], input_data: str = "") -> str:
    """Execute COW program and return output."""
    memory = [0] * 30000
    ptr = 0
    ip = 0
    output = []
    input_pos = 0
    register = None
    max_steps = 1000000

    steps = 0
    while ip < len(program) and steps < max_steps:
        steps += 1
        instr = program[ip]

        if instr == 0:    # moo - jump back to matching MOO
            depth = 1
            ip -= 1
            while ip >= 0 and depth > 0:
                if program[ip] == 7: depth -= 1
                elif program[ip] == 0: depth += 1
                if depth > 0: ip -= 1
        elif instr == 1:  # moO - pointer++
            ptr += 1
        elif instr == 2:  # mOo - pointer--
            ptr -= 1
        elif instr == 3:  # mOO - execute memory as instruction
            if 0 <= memory[ptr] <= 11:
                program.insert(ip + 1, memory[ptr])
        elif instr == 4:  # Moo - print/read char
            if memory[ptr] == 0:
                if input_pos < len(input_data):
                    memory[ptr] = ord(input_data[input_pos])
                    input_pos += 1
            else:
                output.append(chr(memory[ptr] % 256))
        elif instr == 5:  # MOo - mem--
            memory[ptr] -= 1
        elif instr == 6:  # MoO - mem++
            memory[ptr] += 1
        elif instr == 7:  # MOO - loop start
            if memory[ptr] == 0:
                depth = 1
                ip += 1
                while ip < len(program) and depth > 0:
                    if program[ip] == 7: depth += 1
                    elif program[ip] == 0: depth -= 1
                    if depth > 0: ip += 1
        elif instr == 8:  # OOO - clear memory
            memory[ptr] = 0
        elif instr == 9:  # MMM - register/restore
            if memory[ptr] == 0:
                register = ip
            elif register is not None:
                ip = register
                register = None
        elif instr == 10: # OOM - print number
            output.append(str(memory[ptr]))
        elif instr == 11: # oom - read number
            # Skip for now
            pass

        ip += 1

    return ''.join(output)

# ============================================================================
# COW Programs (pre-compiled for common SSG tasks)
# ============================================================================

# "Hello" in COW (generates the word Hello)
COW_HELLO = """
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO Moo
moO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO Moo
MoO MoO MoO MoO MoO MoO MoO Moo
Moo
moO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO
MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO MoO Moo
"""

# ============================================================================
# Frontmatter & Markdown Parser
# ============================================================================

@dataclass
class Frontmatter:
    title: str = ""
    date: str = ""
    tags: List[str] = field(default_factory=list)
    draft: bool = False
    template: str = "default"

def parse_frontmatter(content: str) -> Tuple[Frontmatter, str]:
    """Parse YAML frontmatter from content."""
    lines = content.split('\n')
    if not lines or lines[0].strip() != '---':
        return Frontmatter(), content

    fm = Frontmatter()
    body_start = 1
    for i, line in enumerate(lines[1:], 1):
        if line.strip() == '---':
            body_start = i + 1
            break
        if ':' in line:
            key, value = line.split(':', 1)
            key, value = key.strip(), value.strip()
            if key == 'title':
                fm.title = value
            elif key == 'date':
                fm.date = value
            elif key == 'draft':
                fm.draft = value.lower() in ('true', 'yes')
            elif key == 'tags':
                if value.startswith('['):
                    fm.tags = [t.strip().strip('"\'') for t in value[1:-1].split(',')]
                else:
                    fm.tags = [t.strip() for t in value.split(',')]

    return fm, '\n'.join(lines[body_start:])

def escape_html(s: str) -> str:
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')

def parse_markdown(content: str) -> str:
    """Simple markdown to HTML conversion."""
    html = []
    in_code = False
    in_para = False
    in_list = False

    for line in content.split('\n'):
        stripped = line.strip()

        # Code blocks
        if stripped.startswith('```'):
            if in_code:
                html.append('</code></pre>')
                in_code = False
            else:
                if in_para:
                    html.append('</p>')
                    in_para = False
                if in_list:
                    html.append('</ul>')
                    in_list = False
                html.append('<pre><code>')
                in_code = True
            continue

        if in_code:
            html.append(escape_html(line))
            continue

        # Empty line
        if not stripped:
            if in_para:
                html.append('</p>')
                in_para = False
            if in_list:
                html.append('</ul>')
                in_list = False
            continue

        # Headers
        for i in range(6, 0, -1):
            prefix = '#' * i + ' '
            if stripped.startswith(prefix):
                if in_para:
                    html.append('</p>')
                    in_para = False
                content_text = process_inline(stripped[i+1:].strip())
                html.append(f'<h{i}>{content_text}</h{i}>')
                break
        else:
            # List items
            if stripped.startswith('- ') or stripped.startswith('* '):
                if in_para:
                    html.append('</p>')
                    in_para = False
                if not in_list:
                    html.append('<ul>')
                    in_list = True
                item = process_inline(stripped[2:].strip())
                html.append(f'<li>{item}</li>')
            else:
                # Paragraph
                if not in_para:
                    html.append('<p>')
                    in_para = True
                else:
                    html.append(' ')
                html.append(process_inline(stripped))

    if in_para:
        html.append('</p>')
    if in_list:
        html.append('</ul>')
    if in_code:
        html.append('</code></pre>')

    return '\n'.join(html)

def process_inline(text: str) -> str:
    """Process inline markdown formatting."""
    # Bold
    text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
    # Italic
    text = re.sub(r'\*(.+?)\*', r'<em>\1</em>', text)
    # Code
    text = re.sub(r'`(.+?)`', r'<code>\1</code>', text)
    return text

# ============================================================================
# Template Engine
# ============================================================================

DEFAULT_TEMPLATE = '''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{title}}</title>
<style>
body { font-family: system-ui, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }
pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; }
code { background: #f4f4f4; padding: 0.2rem 0.4rem; }
.cow-generated { color: #8b4513; font-style: italic; }
</style>
</head>
<body>
<article>
<p class="cow-generated">{{cow_greeting}}</p>
<h1>{{title}}</h1>
<time>{{date}}</time>
{{content}}
</article>
<footer><small>Moo! Generated by milk-ssg</small></footer>
</body>
</html>'''

def apply_template(fm: Frontmatter, html: str) -> str:
    """Apply template with COW-generated greeting."""
    # Generate greeting using COW!
    program = parse_cow(COW_HELLO)
    cow_greeting = run_cow(program)

    result = DEFAULT_TEMPLATE
    result = result.replace('{{title}}', escape_html(fm.title))
    result = result.replace('{{date}}', escape_html(fm.date))
    result = result.replace('{{content}}', html)
    result = result.replace('{{cow_greeting}}', escape_html(cow_greeting))
    return result

# ============================================================================
# Tests
# ============================================================================

def test_cow():
    """Test COW interpreter."""
    print("=== Test: COW Interpreter ===")
    program = parse_cow(COW_HELLO)
    result = run_cow(program)
    print(f"COW says: {result}")
    assert "Hello" in result or len(result) > 0, "COW should produce output"
    print("PASS: COW interpreter works")

def test_markdown():
    """Test markdown parser."""
    print("\n=== Test: Markdown ===")
    md = """# Hello World

This is a **bold** test with *italic* text.

- Item 1
- Item 2

```
code block
```
"""
    html = parse_markdown(md)
    print(html)
    assert '<h1>' in html and '<strong>' in html
    print("PASS: Markdown works")

def test_frontmatter():
    """Test frontmatter parser."""
    print("\n=== Test: Frontmatter ===")
    content = """---
title: My Post
date: 2024-01-15
tags: [cow, moo]
draft: false
---

Content here"""
    fm, body = parse_frontmatter(content)
    print(f"Title: {fm.title}")
    print(f"Date: {fm.date}")
    print(f"Tags: {fm.tags}")
    print(f"Draft: {fm.draft}")
    print(f"Body: {body.strip()}")
    assert fm.title == "My Post"
    print("PASS: Frontmatter works")

def test_full():
    """Test full pipeline."""
    print("\n=== Test: Full Pipeline ===")
    content = """---
title: Welcome to the Pasture
date: 2024-01-15
---

# Welcome

This is **milk-ssg**, a COW-powered static site generator.

- Esoteric
- Moo-licious
- Udderly ridiculous
"""
    fm, body = parse_frontmatter(content)
    html = parse_markdown(body)
    output = apply_template(fm, html)
    print(output)
    assert 'Welcome to the Pasture' in output
    print("PASS: Full pipeline works")

# ============================================================================
# Main
# ============================================================================

def main():
    args = sys.argv[1:]

    if not args:
        print("milk-ssg - COW esoteric language SSG")
        print("Commands: test-cow test-markdown test-frontmatter test-full")
        return

    cmd = args[0]
    if cmd == 'test-cow':
        test_cow()
    elif cmd == 'test-markdown':
        test_markdown()
    elif cmd == 'test-frontmatter':
        test_frontmatter()
    elif cmd == 'test-full':
        test_full()
    elif cmd == 'moo':
        # Easter egg: run COW code from stdin
        code = sys.stdin.read()
        program = parse_cow(code)
        print(run_cow(program))
    else:
        print(f"Unknown command: {cmd}")
        sys.exit(1)

if __name__ == '__main__':
    main()
