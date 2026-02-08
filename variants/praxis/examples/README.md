# Examples

Example configurations, templates, and content for doit-ssg.

## Structure

```
examples/
├── config/             # SSG configuration examples
│   ├── zola.config.json
│   └── hakyll.config.json
├── templates/          # HTML template examples
│   ├── base.html
│   └── post.html
├── content/            # Example content
│   ├── index.md
│   └── posts/
│       └── first-post.md
└── README.md           # This file
```

## Usage

### Configuration

Each SSG has its own configuration format. The examples in `config/` show
how to configure common options.

### Templates

Templates in `templates/` are generic examples that work with most SSGs.
Adjust the syntax based on your chosen SSG's template engine:

- Zola: Tera templates
- Hakyll: Pandoc + custom routes
- Jekyll: Liquid templates
- etc.

### Content

Content files use Markdown with YAML frontmatter:

```markdown
---
title: Page Title
date: 2025-12-22
template: base.html
---

# Content here
```

## Adapting for Your SSG

1. Copy the example files to your project
2. Adjust template syntax for your SSG
3. Update configuration paths
4. Run your SSG's build command
