// WokeLang SSG FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// All types and layouts must match the Idris2 ABI definitions.
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

const std = @import("std");

// Version information (keep in sync with Cargo.toml)
const VERSION = "0.1.0";
const BUILD_INFO = "wokelang-ssg built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

/// SSG Engine State
pub const SsgState = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
    content_dir: ?[]const u8,
    output_dir: ?[]const u8,
};

/// Library handle (opaque to C)
pub const Handle = opaque {};

fn handleToState(handle: *Handle) *SsgState {
    return @ptrCast(@alignCast(handle));
}

fn stateToHandle(state: *SsgState) *Handle {
    return @ptrCast(@alignCast(state));
}

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the SSG engine
/// Returns a handle (as u64 pointer), or null on failure
export fn wokelang_ssg_init() u64 {
    const allocator = std.heap.c_allocator;

    const state = allocator.create(SsgState) catch {
        setError("Failed to allocate SSG state");
        return 0;
    };

    state.* = .{
        .allocator = allocator,
        .initialized = true,
        .content_dir = null,
        .output_dir = null,
    };

    clearError();
    const handle = stateToHandle(state);
    return @intFromPtr(handle);
}

/// Free the SSG engine
export fn wokelang_ssg_free(handle_ptr: u64) void {
    if (handle_ptr == 0) return;

    const handle: *Handle = @ptrFromInt(handle_ptr);
    const state = handleToState(handle);
    const allocator = state.allocator;

    // Free allocated strings
    if (state.content_dir) |dir| allocator.free(dir);
    if (state.output_dir) |dir| allocator.free(dir);

    state.initialized = false;
    allocator.destroy(state);
    clearError();
}

//==============================================================================
// Core Operations
//==============================================================================

/// Build the static site
/// Returns 0 on success, non-zero on error
export fn wokelang_ssg_build(
    handle_ptr: u64,
    source: [*:0]const u8,
    output: [*:0]const u8,
) u32 {
    if (handle_ptr == 0) {
        setError("Null handle");
        return @intFromEnum(Result.null_pointer);
    }

    const handle: *Handle = @ptrFromInt(handle_ptr);
    const state = handleToState(handle);

    if (!state.initialized) {
        setError("SSG not initialized");
        return @intFromEnum(Result.@"error");
    }

    // Store paths
    const source_slice = std.mem.span(source);
    const output_slice = std.mem.span(output);

    state.content_dir = state.allocator.dupe(u8, source_slice) catch {
        setError("Failed to allocate source path");
        return @intFromEnum(Result.out_of_memory);
    };

    state.output_dir = state.allocator.dupe(u8, output_slice) catch {
        setError("Failed to allocate output path");
        return @intFromEnum(Result.out_of_memory);
    };

    // Note: Actual build logic would call into Rust SSG here
    // For now, this is a stub that returns success
    clearError();
    return @intFromEnum(Result.ok);
}

//==============================================================================
// String Operations
//==============================================================================

/// Parse markdown to HTML
/// Returns allocated HTML string, caller must free
export fn wokelang_ssg_parse_markdown(
    handle_ptr: u64,
    markdown: [*:0]const u8,
) [*:0]const u8 {
    if (handle_ptr == 0) {
        setError("Null handle");
        // Return empty string on error
        const allocator = std.heap.c_allocator;
        return (allocator.dupeZ(u8, "") catch return @ptrCast(&[_]u8{0})).ptr;
    }

    const handle: *Handle = @ptrFromInt(handle_ptr);
    const state = handleToState(handle);

    if (!state.initialized) {
        setError("SSG not initialized");
        const allocator = std.heap.c_allocator;
        return (allocator.dupeZ(u8, "") catch return @ptrCast(&[_]u8{0})).ptr;
    }

    const md_slice = std.mem.span(markdown);

    // Note: Actual markdown parsing would call into pulldown-cmark via Rust
    // For now, wrap in <p> tags as a stub
    const html = std.fmt.allocPrintZ(
        state.allocator,
        "<p>{s}</p>",
        .{md_slice},
    ) catch {
        setError("Failed to parse markdown");
        return (state.allocator.dupeZ(u8, "") catch return @ptrCast(&[_]u8{0})).ptr;
    };

    clearError();
    return html.ptr;
}

/// Free a string allocated by the library
export fn wokelang_ssg_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;
    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Array/Buffer Operations
//==============================================================================

/// Process an array of data
export fn {{project}}_process_array(
    handle: ?*Handle,
    buffer: ?[*]const u8,
    len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const buf = buffer orelse {
        setError("Null buffer");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Access the buffer
    const data = buf[0..len];
    _ = data;

    // Process data here

    clearError();
    return .ok;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn wokelang_ssg_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn wokelang_ssg_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn wokelang_ssg_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Callback Support
//==============================================================================

/// Callback function type (C ABI)
pub const Callback = *const fn (u64, u32) callconv(.C) u32;

/// Register a callback
export fn {{project}}_register_callback(
    handle: ?*Handle,
    callback: ?Callback,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const cb = callback orelse {
        setError("Null callback");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Store callback for later use
    _ = cb;

    clearError();
    return .ok;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn {{project}}_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle_ptr = wokelang_ssg_init();
    defer wokelang_ssg_free(handle_ptr);

    try std.testing.expect(handle_ptr != 0);
}

test "build" {
    const handle_ptr = wokelang_ssg_init();
    defer wokelang_ssg_free(handle_ptr);

    const result = wokelang_ssg_build(handle_ptr, "content", "public");
    try std.testing.expectEqual(@as(u32, 0), result);
}

test "parse markdown" {
    const handle_ptr = wokelang_ssg_init();
    defer wokelang_ssg_free(handle_ptr);

    const html = wokelang_ssg_parse_markdown(handle_ptr, "Hello");
    defer wokelang_ssg_free_string(html);

    const html_str = std.mem.span(html);
    try std.testing.expect(html_str.len > 0);
}

test "version" {
    const ver = wokelang_ssg_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}
