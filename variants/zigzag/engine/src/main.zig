// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// zigzag-ssg: High-performance static site generator in Zig
// Zero-overhead abstractions and compile-time templates

const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const Allocator = std.mem.Allocator;

/// Page metadata
pub const PageMeta = struct {
    title: []const u8,
    path: []const u8,
    template: []const u8 = "default",
};

/// A page with content
pub const Page = struct {
    meta: PageMeta,
    content: []const u8,

    pub fn render(self: *const Page, allocator: Allocator) ![]u8 {
        const html = try std.fmt.allocPrint(allocator,
            \\<!DOCTYPE html>
            \\<html lang="en">
            \\<head>
            \\    <meta charset="UTF-8">
            \\    <title>{s}</title>
            \\</head>
            \\<body>
            \\    <article>
            \\        <h1>{s}</h1>
            \\        {s}
            \\    </article>
            \\</body>
            \\</html>
        , .{
            escapeHtml(allocator, self.meta.title) catch self.meta.title,
            escapeHtml(allocator, self.meta.title) catch self.meta.title,
            self.content,
        });
        return html;
    }
};

/// Site configuration
pub const Site = struct {
    name: []const u8,
    output_dir: []const u8 = "_site",
    pages: std.ArrayList(Page),

    pub fn init(allocator: Allocator, name: []const u8) Site {
        return .{
            .name = name,
            .pages = std.ArrayList(Page).init(allocator),
        };
    }

    pub fn deinit(self: *Site) void {
        self.pages.deinit();
    }

    pub fn addPage(self: *Site, page: Page) !void {
        try self.pages.append(page);
    }

    pub fn build(self: *const Site, allocator: Allocator) !void {
        // Create output directory
        fs.cwd().makeDir(self.output_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        std.debug.print("zigzag-ssg: Building site '{s}'...\n", .{self.name});

        for (self.pages.items) |page| {
            const html = try page.render(allocator);
            defer allocator.free(html);

            const out_path = try std.fmt.allocPrint(
                allocator,
                "{s}/{s}.html",
                .{ self.output_dir, page.meta.path },
            );
            defer allocator.free(out_path);

            const file = try fs.cwd().createFile(out_path, .{});
            defer file.close();
            try file.writeAll(html);

            std.debug.print("zigzag-ssg: Built {s}\n", .{out_path});
        }

        std.debug.print("zigzag-ssg: Built {d} pages\n", .{self.pages.items.len});
    }
};

/// Escape HTML special characters
fn escapeHtml(allocator: Allocator, text: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    for (text) |c| {
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

/// Parse simple Markdown to HTML
pub fn parseMarkdown(allocator: Allocator, markdown: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    var lines = mem.splitSequence(u8, markdown, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        if (mem.startsWith(u8, line, "# ")) {
            try result.appendSlice("<h1>");
            try result.appendSlice(line[2..]);
            try result.appendSlice("</h1>\n");
        } else if (mem.startsWith(u8, line, "## ")) {
            try result.appendSlice("<h2>");
            try result.appendSlice(line[3..]);
            try result.appendSlice("</h2>\n");
        } else if (mem.startsWith(u8, line, "- ") or mem.startsWith(u8, line, "* ")) {
            try result.appendSlice("<li>");
            try result.appendSlice(line[2..]);
            try result.appendSlice("</li>\n");
        } else {
            try result.appendSlice("<p>");
            try result.appendSlice(line);
            try result.appendSlice("</p>\n");
        }
    }

    return result.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var site = Site.init(allocator, "zigzag-site");
    defer site.deinit();

    // Example page
    try site.addPage(.{
        .meta = .{ .title = "Home", .path = "index" },
        .content = "<p>Welcome to zigzag-ssg!</p>",
    });

    try site.build(allocator);
}

test "escape html" {
    const allocator = std.testing.allocator;
    const result = try escapeHtml(allocator, "<script>alert('xss')</script>");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("&lt;script&gt;alert('xss')&lt;/script&gt;", result);
}
