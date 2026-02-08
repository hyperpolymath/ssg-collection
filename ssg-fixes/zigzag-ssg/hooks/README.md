# Hooks System

Build hooks for zigzag-ssg automation.

## Planned Hooks

| Hook | Trigger | Purpose |
|------|---------|---------|
| `pre-build.zig` | Before build | Validate content, check dependencies |
| `post-build.zig` | After build | Deploy, notify, cleanup |
| `watch.zig` | File change | Incremental rebuild |
| `deploy.zig` | Release | Push to hosting |

## Status

Hooks are planned for **v1.3** - see STATE.scm roadmap.

## Usage (Future)

```bash
# Run pre-build hook
zig build run-hook -- pre-build

# Run post-build hook
zig build run-hook -- post-build
```

## Writing Hooks

Hooks are Zig modules that export a `run` function:

```zig
// hooks/pre-build.zig
const std = @import("std");

pub fn run(allocator: std.mem.Allocator) !void {
    // Your hook logic here
    std.debug.print("Pre-build hook running...\n", .{});
}
```

---

*Part of zigzag-ssg hooks system*
