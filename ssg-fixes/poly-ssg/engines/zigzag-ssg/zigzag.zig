// zigzag.zig - Zig-powered static site generator
//
// "ZigZag" - Fast and safe site generation
//
// Zig's compile-time features and safety make for efficient SSG code.
// Zero hidden allocations, explicit error handling.

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// Types
// ============================================================================

const Frontmatter = struct {
    title: []const u8 = "",
    date: []const u8 = "",
    draft: bool = false,
    template: []const u8 = "default",
};

const ParserState = struct {
    html: std.ArrayList(u8),
    in_para: bool = false,
    in_code: bool = false,
    in_list: bool = false,
};

// ============================================================================
// String Utilities
// ============================================================================

fn startsWith(str: []const u8, prefix: []const u8) bool {
    if (str.len < prefix.len) return false;
    return std.mem.eql(u8, str[0..prefix.len], prefix);
}

fn trim(str: []const u8) []const u8 {
    var start: usize = 0;
    var end: usize = str.len;

    while (start < end and (str[start] == ' ' or str[start] == '\t' or str[start] == '\n' or str[start] == '\r')) {
        start += 1;
    }
    while (end > start and (str[end - 1] == ' ' or str[end - 1] == '\t' or str[end - 1] == '\n' or str[end - 1] == '\r')) {
        end -= 1;
    }
    return str[start..end];
}

fn escapeHtml(allocator: Allocator, str: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    for (str) |c| {
        switch (c) {
            '<' => try result.appendSlice("&lt;"),
            '>' => try result.appendSlice("&gt;"),
            '&' => try result.appendSlice("&amp;"),
            '"' => try result.appendSlice("&quot;"),
            else => try result.append(c),
        }
    }
    return result.toOwnedSlice();
}

// ============================================================================
// Frontmatter Parser
// ============================================================================

fn parseFrontmatter(content: []const u8) struct { fm: Frontmatter, body: []const u8 } {
    var lines = std.mem.split(u8, content, "\n");

    const first = lines.next() orelse return .{ .fm = Frontmatter{}, .body = content };
    if (!std.mem.eql(u8, trim(first), "---")) {
        return .{ .fm = Frontmatter{}, .body = content };
    }

    var fm = Frontmatter{};
    var body_start: usize = first.len + 1;

    while (lines.next()) |line| {
        body_start += line.len + 1;
        const trimmed = trim(line);

        if (std.mem.eql(u8, trimmed, "---")) {
            const body = if (body_start < content.len) content[body_start..] else "";
            return .{ .fm = fm, .body = body };
        }

        // Parse key: value
        if (std.mem.indexOf(u8, line, ":")) |colon_idx| {
            const key = trim(line[0..colon_idx]);
            const value = trim(line[colon_idx + 1..]);

            if (std.mem.eql(u8, key, "title")) {
                fm.title = value;
            } else if (std.mem.eql(u8, key, "date")) {
                fm.date = value;
            } else if (std.mem.eql(u8, key, "draft")) {
                fm.draft = std.mem.eql(u8, value, "true") or std.mem.eql(u8, value, "yes");
            } else if (std.mem.eql(u8, key, "template")) {
                fm.template = value;
            }
        }
    }

    return .{ .fm = fm, .body = "" };
}

// ============================================================================
// Markdown Parser
// ============================================================================

fn processLine(state: *ParserState, line: []const u8) !void {
    const trimmed = trim(line);

    // Code fence
    if (startsWith(trimmed, "```")) {
        if (state.in_code) {
            try state.html.appendSlice("</code></pre>\n");
            state.in_code = false;
        } else {
            if (state.in_para) {
                try state.html.appendSlice("</p>\n");
                state.in_para = false;
            }
            if (state.in_list) {
                try state.html.appendSlice("</ul>\n");
                state.in_list = false;
            }
            try state.html.appendSlice("<pre><code>");
            state.in_code = true;
        }
        return;
    }

    // Inside code block
    if (state.in_code) {
        for (line) |c| {
            switch (c) {
                '<' => try state.html.appendSlice("&lt;"),
                '>' => try state.html.appendSlice("&gt;"),
                '&' => try state.html.appendSlice("&amp;"),
                else => try state.html.append(c),
            }
        }
        try state.html.append('\n');
        return;
    }

    // Empty line
    if (trimmed.len == 0) {
        if (state.in_para) {
            try state.html.appendSlice("</p>\n");
            state.in_para = false;
        }
        if (state.in_list) {
            try state.html.appendSlice("</ul>\n");
            state.in_list = false;
        }
        return;
    }

    // Headers
    if (startsWith(trimmed, "### ")) {
        try state.html.appendSlice("<h3>");
        try state.html.appendSlice(trimmed[4..]);
        try state.html.appendSlice("</h3>\n");
        return;
    }
    if (startsWith(trimmed, "## ")) {
        try state.html.appendSlice("<h2>");
        try state.html.appendSlice(trimmed[3..]);
        try state.html.appendSlice("</h2>\n");
        return;
    }
    if (startsWith(trimmed, "# ")) {
        try state.html.appendSlice("<h1>");
        try state.html.appendSlice(trimmed[2..]);
        try state.html.appendSlice("</h1>\n");
        return;
    }

    // List items
    if (startsWith(trimmed, "- ") or startsWith(trimmed, "* ")) {
        if (state.in_para) {
            try state.html.appendSlice("</p>\n");
            state.in_para = false;
        }
        if (!state.in_list) {
            try state.html.appendSlice("<ul>\n");
            state.in_list = true;
        }
        try state.html.appendSlice("<li>");
        try state.html.appendSlice(trimmed[2..]);
        try state.html.appendSlice("</li>\n");
        return;
    }

    // Paragraph
    if (!state.in_para) {
        try state.html.appendSlice("<p>");
        state.in_para = true;
    } else {
        try state.html.append(' ');
    }
    try state.html.appendSlice(trimmed);
}

fn parseMarkdown(allocator: Allocator, content: []const u8) ![]u8 {
    var state = ParserState{
        .html = std.ArrayList(u8).init(allocator),
    };

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        try processLine(&state, line);
    }

    // Close open tags
    if (state.in_para) {
        try state.html.appendSlice("</p>\n");
    }
    if (state.in_list) {
        try state.html.appendSlice("</ul>\n");
    }
    if (state.in_code) {
        try state.html.appendSlice("</code></pre>\n");
    }

    return state.html.toOwnedSlice();
}

// ============================================================================
// Template Engine
// ============================================================================

const TEMPLATE_START = "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>";
const TEMPLATE_MID1 = "</title><style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style></head><body><article><h1>";
const TEMPLATE_MID2 = "</h1><time>";
const TEMPLATE_MID3 = "</time>";
const TEMPLATE_END = "</article></body></html>";

fn applyTemplate(allocator: Allocator, fm: Frontmatter, html: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    try result.appendSlice(TEMPLATE_START);
    try result.appendSlice(fm.title);
    try result.appendSlice(TEMPLATE_MID1);
    try result.appendSlice(fm.title);
    try result.appendSlice(TEMPLATE_MID2);
    try result.appendSlice(fm.date);
    try result.appendSlice(TEMPLATE_MID3);
    try result.appendSlice(html);
    try result.appendSlice(TEMPLATE_END);
    return result.toOwnedSlice();
}

// ============================================================================
// Tests
// ============================================================================

fn testMarkdown(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Test: Markdown ===\n", .{});

    const md = "# Hello World\n\nThis is a test.\n\n- Item 1\n- Item 2\n\n```\ncode block\n```\n";
    const html = try parseMarkdown(allocator, md);
    try stdout.print("{s}\n", .{html});
}

fn testFrontmatter() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Test: Frontmatter ===\n", .{});

    const content = "---\ntitle: My Post\ndate: 2024-01-15\ndraft: false\n---\n\nContent here\n";
    const result = parseFrontmatter(content);

    try stdout.print("Title: {s}\n", .{result.fm.title});
    try stdout.print("Date: {s}\n", .{result.fm.date});
    try stdout.print("Draft: {}\n", .{result.fm.draft});
    try stdout.print("Body: {s}\n", .{result.body});
}

fn testFull(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("=== Test: Full Pipeline ===\n", .{});

    const content = "---\ntitle: Welcome\ndate: 2024-01-15\n---\n\n# Welcome\n\nThis is ZigZag, a Zig SSG.\n\n- Fast\n- Safe\n- Simple\n";
    const result = parseFrontmatter(content);
    const html = try parseMarkdown(allocator, result.body);
    const output = try applyTemplate(allocator, result.fm, html);

    try stdout.print("{s}\n", .{output});
}

// ============================================================================
// Main
// ============================================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const stdout = std.io.getStdOut().writer();

    if (args.len < 2) {
        try stdout.print("ZigZag SSG - Zig powered\n", .{});
        try stdout.print("Commands: test-markdown test-frontmatter test-full\n", .{});
        return;
    }

    if (std.mem.eql(u8, args[1], "test-markdown")) {
        try testMarkdown(allocator);
    } else if (std.mem.eql(u8, args[1], "test-frontmatter")) {
        try testFrontmatter();
    } else if (std.mem.eql(u8, args[1], "test-full")) {
        try testFull(allocator);
    } else {
        try stdout.print("Unknown command: {s}\n", .{args[1]});
    }
}
