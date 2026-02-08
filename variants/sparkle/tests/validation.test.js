// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// validation.test.js — Unit tests for validation module

import { assertEquals, assertFalse, assert } from "@std/assert";
import {
  isValidPath,
  isValidArgument,
  isValidUrl,
  isValidPort,
  isValidInterface,
  sanitizePath,
} from "../adapters/validation.js";

// ═══════════════════════════════════════════════════════════════════════════════
// isValidPath Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("isValidPath - accepts valid paths", () => {
  assert(isValidPath("/home/user/project"));
  assert(isValidPath("relative/path"));
  assert(isValidPath("file.txt"));
  assert(isValidPath("path/to/file.js"));
});

Deno.test("isValidPath - rejects null bytes", () => {
  assertFalse(isValidPath("/path/with\0null"));
  assertFalse(isValidPath("file\0.txt"));
});

Deno.test("isValidPath - rejects shell metacharacters", () => {
  assertFalse(isValidPath("/path;rm -rf /"));
  assertFalse(isValidPath("file|cat /etc/passwd"));
  assertFalse(isValidPath("$(whoami)"));
  assertFalse(isValidPath("`id`"));
  assertFalse(isValidPath("file&bg"));
});

Deno.test("isValidPath - rejects path traversal", () => {
  assertFalse(isValidPath("../../../etc/passwd"));
  assertFalse(isValidPath("/var/../../etc/passwd"));
});

Deno.test("isValidPath - rejects empty and non-string", () => {
  assertFalse(isValidPath(""));
  assertFalse(isValidPath(null));
  assertFalse(isValidPath(undefined));
  assertFalse(isValidPath(123));
});

Deno.test("isValidPath - rejects excessively long paths", () => {
  const longPath = "a".repeat(5000);
  assertFalse(isValidPath(longPath));
});

// ═══════════════════════════════════════════════════════════════════════════════
// isValidArgument Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("isValidArgument - accepts valid arguments", () => {
  assert(isValidArgument("--version"));
  assert(isValidArgument("-v"));
  assert(isValidArgument("build"));
  assert(isValidArgument("my-project"));
});

Deno.test("isValidArgument - rejects injection attempts", () => {
  assertFalse(isValidArgument("; rm -rf /"));
  assertFalse(isValidArgument("$(cat /etc/passwd)"));
  assertFalse(isValidArgument("`id`"));
});

Deno.test("isValidArgument - rejects null bytes", () => {
  assertFalse(isValidArgument("arg\0injection"));
});

// ═══════════════════════════════════════════════════════════════════════════════
// isValidUrl Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("isValidUrl - accepts http and https URLs", () => {
  assert(isValidUrl("https://example.com"));
  assert(isValidUrl("http://localhost:8080"));
  assert(isValidUrl("https://example.com/path?query=value"));
});

Deno.test("isValidUrl - rejects non-http protocols", () => {
  assertFalse(isValidUrl("file:///etc/passwd"));
  assertFalse(isValidUrl("javascript:alert(1)"));
  assertFalse(isValidUrl("data:text/html,<script>alert(1)</script>"));
});

Deno.test("isValidUrl - rejects invalid URLs", () => {
  assertFalse(isValidUrl("not-a-url"));
  assertFalse(isValidUrl(""));
  assertFalse(isValidUrl(null));
});

// ═══════════════════════════════════════════════════════════════════════════════
// isValidPort Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("isValidPort - accepts valid ports", () => {
  assert(isValidPort(80));
  assert(isValidPort(443));
  assert(isValidPort(8080));
  assert(isValidPort(1));
  assert(isValidPort(65535));
});

Deno.test("isValidPort - rejects invalid ports", () => {
  assertFalse(isValidPort(0));
  assertFalse(isValidPort(-1));
  assertFalse(isValidPort(65536));
  assertFalse(isValidPort(100000));
});

Deno.test("isValidPort - rejects non-numbers", () => {
  assertFalse(isValidPort("8080"));
  assertFalse(isValidPort(null));
  assertFalse(isValidPort(undefined));
});

Deno.test("isValidPort - rejects floats", () => {
  assertFalse(isValidPort(80.5));
  assertFalse(isValidPort(3.14));
});

// ═══════════════════════════════════════════════════════════════════════════════
// isValidInterface Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("isValidInterface - accepts valid interfaces", () => {
  assert(isValidInterface("localhost"));
  assert(isValidInterface("0.0.0.0"));
  assert(isValidInterface("127.0.0.1"));
  assert(isValidInterface("192.168.1.1"));
  assert(isValidInterface("example.com"));
});

Deno.test("isValidInterface - rejects invalid interfaces", () => {
  assertFalse(isValidInterface(""));
  assertFalse(isValidInterface(null));
  assertFalse(isValidInterface("a".repeat(300)));
});

// ═══════════════════════════════════════════════════════════════════════════════
// sanitizePath Tests
// ═══════════════════════════════════════════════════════════════════════════════

Deno.test("sanitizePath - normalizes valid paths", () => {
  const result = sanitizePath("/base", "subdir/file.txt");
  assertEquals(result, "/base/subdir/file.txt");
});

Deno.test("sanitizePath - handles absolute paths", () => {
  const result = sanitizePath("/base", "/absolute/path");
  assertEquals(result, "/absolute/path");
});

Deno.test("sanitizePath - returns null for invalid paths", () => {
  assertEquals(sanitizePath("/base", "../../../etc/passwd"), null);
  assertEquals(sanitizePath("/base", "path;injection"), null);
});
