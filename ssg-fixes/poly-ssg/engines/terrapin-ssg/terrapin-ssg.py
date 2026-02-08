#!/usr/bin/env python3
"""
terrapin-ssg - Logo-powered static site generator for kids

A friendly SSG that turns turtle graphics into web pages!
Kids can write Logo programs that become SVG images in their posts.

Supports basic Logo commands:
  FORWARD n (FD)   - Move forward n steps
  BACK n (BK)      - Move backward n steps
  RIGHT n (RT)     - Turn right n degrees
  LEFT n (LT)      - Turn left n degrees
  PENUP (PU)       - Stop drawing
  PENDOWN (PD)     - Start drawing
  HOME             - Return to center
  REPEAT n [...]   - Repeat commands n times
  SETCOLOR c       - Set pen color
"""

import sys
import os
import re
import math
from typing import List, Tuple, Dict, Optional
from dataclasses import dataclass, field

# ============================================================================
# Logo (Turtle Graphics) Interpreter
# ============================================================================

@dataclass
class Turtle:
    x: float = 250.0      # Start in center of 500x500 canvas
    y: float = 250.0
    angle: float = -90.0  # Facing up (north)
    pen_down: bool = True
    color: str = "#228B22"  # Forest green
    paths: List[str] = field(default_factory=list)
    current_path: List[Tuple[float, float]] = field(default_factory=list)

    def start_path(self):
        if self.pen_down and not self.current_path:
            self.current_path = [(self.x, self.y)]

    def end_path(self):
        if len(self.current_path) > 1:
            d = "M " + " L ".join(f"{p[0]:.1f},{p[1]:.1f}" for p in self.current_path)
            self.paths.append(f'<path d="{d}" stroke="{self.color}" fill="none" stroke-width="2"/>')
        self.current_path = []

    def forward(self, distance: float):
        rad = math.radians(self.angle)
        new_x = self.x + distance * math.cos(rad)
        new_y = self.y + distance * math.sin(rad)
        if self.pen_down:
            self.start_path()
            self.current_path.append((new_x, new_y))
        self.x, self.y = new_x, new_y

    def back(self, distance: float):
        self.forward(-distance)

    def right(self, angle: float):
        self.angle = (self.angle + angle) % 360

    def left(self, angle: float):
        self.angle = (self.angle - angle) % 360

    def penup(self):
        self.end_path()
        self.pen_down = False

    def pendown(self):
        self.pen_down = True
        self.start_path()

    def home(self):
        self.end_path()
        self.x, self.y = 250.0, 250.0
        self.angle = -90.0
        self.start_path()

    def setcolor(self, color: str):
        self.end_path()
        colors = {
            'red': '#FF0000', 'green': '#00FF00', 'blue': '#0000FF',
            'yellow': '#FFFF00', 'orange': '#FFA500', 'purple': '#800080',
            'black': '#000000', 'white': '#FFFFFF', 'pink': '#FFC0CB',
            'brown': '#8B4513', 'cyan': '#00FFFF', 'magenta': '#FF00FF',
        }
        self.color = colors.get(color.lower(), color)
        self.start_path()

    def to_svg(self) -> str:
        self.end_path()
        paths = '\n  '.join(self.paths)
        return f'''<svg viewBox="0 0 500 500" xmlns="http://www.w3.org/2000/svg" style="max-width:300px;background:#fffef5;border:2px solid #8B4513;border-radius:8px">
  {paths}
</svg>'''

def tokenize_logo(code: str) -> List[str]:
    """Tokenize Logo code."""
    code = code.upper()
    # Handle brackets for REPEAT
    code = code.replace('[', ' [ ').replace(']', ' ] ')
    return code.split()

def parse_logo(tokens: List[str], turtle: Turtle):
    """Parse and execute Logo tokens."""
    i = 0
    while i < len(tokens):
        cmd = tokens[i]

        if cmd in ('FD', 'FORWARD'):
            if i + 1 < len(tokens):
                turtle.forward(float(tokens[i + 1]))
                i += 2
            else:
                i += 1
        elif cmd in ('BK', 'BACK'):
            if i + 1 < len(tokens):
                turtle.back(float(tokens[i + 1]))
                i += 2
            else:
                i += 1
        elif cmd in ('RT', 'RIGHT'):
            if i + 1 < len(tokens):
                turtle.right(float(tokens[i + 1]))
                i += 2
            else:
                i += 1
        elif cmd in ('LT', 'LEFT'):
            if i + 1 < len(tokens):
                turtle.left(float(tokens[i + 1]))
                i += 2
            else:
                i += 1
        elif cmd in ('PU', 'PENUP'):
            turtle.penup()
            i += 1
        elif cmd in ('PD', 'PENDOWN'):
            turtle.pendown()
            i += 1
        elif cmd == 'HOME':
            turtle.home()
            i += 1
        elif cmd == 'SETCOLOR':
            if i + 1 < len(tokens):
                turtle.setcolor(tokens[i + 1])
                i += 2
            else:
                i += 1
        elif cmd == 'REPEAT':
            if i + 2 < len(tokens):
                count = int(tokens[i + 1])
                # Find matching brackets
                if tokens[i + 2] == '[':
                    depth = 1
                    start = i + 3
                    end = start
                    while end < len(tokens) and depth > 0:
                        if tokens[end] == '[':
                            depth += 1
                        elif tokens[end] == ']':
                            depth -= 1
                        if depth > 0:
                            end += 1
                    # Execute repeat body
                    body = tokens[start:end]
                    for _ in range(count):
                        parse_logo(body, turtle)
                    i = end + 1
                else:
                    i += 2
            else:
                i += 1
        else:
            i += 1

def run_logo(code: str) -> str:
    """Execute Logo code and return SVG."""
    turtle = Turtle()
    tokens = tokenize_logo(code)
    parse_logo(tokens, turtle)
    return turtle.to_svg()

# ============================================================================
# Sample Logo Programs
# ============================================================================

LOGO_SQUARE = "REPEAT 4 [ FD 80 RT 90 ]"
LOGO_STAR = "REPEAT 5 [ FD 100 RT 144 ]"
LOGO_SPIRAL = "REPEAT 36 [ FD 50 RT 170 ]"
LOGO_FLOWER = """
REPEAT 6 [
  SETCOLOR RED
  REPEAT 4 [ FD 50 RT 90 ]
  RT 60
]
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
    """Parse YAML frontmatter."""
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
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')

def parse_markdown(content: str) -> str:
    """Parse markdown with Logo code blocks."""
    html = []
    in_code = False
    in_logo = False
    in_para = False
    in_list = False
    code_buffer = []

    for line in content.split('\n'):
        stripped = line.strip()

        # Logo code blocks (```logo)
        if stripped.startswith('```logo'):
            if in_para:
                html.append('</p>')
                in_para = False
            in_logo = True
            code_buffer = []
            continue
        elif stripped == '```' and in_logo:
            # Execute Logo and insert SVG
            logo_code = '\n'.join(code_buffer)
            svg = run_logo(logo_code)
            html.append(f'<div class="turtle-art">{svg}</div>')
            in_logo = False
            code_buffer = []
            continue
        elif in_logo:
            code_buffer.append(line)
            continue

        # Regular code blocks
        if stripped.startswith('```'):
            if in_code:
                html.append('</code></pre>')
                in_code = False
            else:
                if in_para:
                    html.append('</p>')
                    in_para = False
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
    """Process inline formatting."""
    text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
    text = re.sub(r'\*(.+?)\*', r'<em>\1</em>', text)
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
<title>{{title}} - Turtle Pages</title>
<style>
body {
  font-family: 'Comic Sans MS', cursive, sans-serif;
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
  line-height: 1.8;
  background: linear-gradient(135deg, #e8f5e9 0%, #fff8e1 100%);
}
h1, h2, h3 { color: #2e7d32; }
pre { background: #fff; padding: 1rem; border: 2px dashed #8BC34A; border-radius: 8px; }
code { background: #fff9c4; padding: 0.2rem 0.4rem; border-radius: 4px; }
.turtle-art {
  display: inline-block;
  margin: 1rem auto;
  padding: 0.5rem;
}
article { background: white; padding: 2rem; border-radius: 16px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
footer { text-align: center; margin-top: 2rem; color: #666; }
</style>
</head>
<body>
<article>
<h1>{{title}}</h1>
<time>{{date}}</time>
{{content}}
</article>
<footer>Made with Terrapin SSG - Learning to code is fun!</footer>
</body>
</html>'''

def apply_template(fm: Frontmatter, html: str) -> str:
    """Apply template."""
    result = DEFAULT_TEMPLATE
    result = result.replace('{{title}}', escape_html(fm.title))
    result = result.replace('{{date}}', escape_html(fm.date))
    result = result.replace('{{content}}', html)
    return result

# ============================================================================
# Tests
# ============================================================================

def test_logo():
    """Test Logo interpreter."""
    print("=== Test: Logo Interpreter ===")
    svg = run_logo(LOGO_SQUARE)
    print(svg[:200] + "...")
    assert 'svg' in svg and 'path' in svg
    print("PASS: Logo interpreter works")

def test_markdown():
    """Test markdown with Logo."""
    print("\n=== Test: Markdown ===")
    md = """# My Drawing

Check out my turtle art:

```logo
REPEAT 4 [ FD 80 RT 90 ]
```

Cool right?
"""
    html = parse_markdown(md)
    print(html[:300] + "...")
    assert '<svg' in html
    print("PASS: Markdown with Logo works")

def test_frontmatter():
    """Test frontmatter."""
    print("\n=== Test: Frontmatter ===")
    content = """---
title: My Turtle Art
date: 2024-01-15
tags: [logo, art]
---

Drawing stuff"""
    fm, body = parse_frontmatter(content)
    print(f"Title: {fm.title}")
    print(f"Date: {fm.date}")
    print(f"Tags: {fm.tags}")
    assert fm.title == "My Turtle Art"
    print("PASS: Frontmatter works")

def test_full():
    """Test full pipeline."""
    print("\n=== Test: Full Pipeline ===")
    content = """---
title: My First Turtle Drawing
date: 2024-01-15
---

# Hello Turtle!

I made a **star** with turtle graphics!

```logo
SETCOLOR BLUE
REPEAT 5 [ FD 100 RT 144 ]
```

And here's a **square**:

```logo
SETCOLOR GREEN
REPEAT 4 [ FD 60 RT 90 ]
```

Turtle graphics are fun!
"""
    fm, body = parse_frontmatter(content)
    html = parse_markdown(body)
    output = apply_template(fm, html)
    print(output[:500] + "...")
    assert 'My First Turtle Drawing' in output
    assert '<svg' in output
    print("PASS: Full pipeline works")

# ============================================================================
# Main
# ============================================================================

def main():
    args = sys.argv[1:]

    if not args:
        print("Terrapin SSG - Logo-powered SSG for kids!")
        print("Commands: test-logo test-markdown test-frontmatter test-full")
        print("\nLogo commands: FD, BK, RT, LT, PU, PD, HOME, REPEAT, SETCOLOR")
        return

    cmd = args[0]
    if cmd == 'test-logo':
        test_logo()
    elif cmd == 'test-markdown':
        test_markdown()
    elif cmd == 'test-frontmatter':
        test_frontmatter()
    elif cmd == 'test-full':
        test_full()
    elif cmd == 'draw':
        # Run Logo from stdin
        code = sys.stdin.read()
        print(run_logo(code))
    else:
        print(f"Unknown command: {cmd}")
        sys.exit(1)

if __name__ == '__main__':
    main()
