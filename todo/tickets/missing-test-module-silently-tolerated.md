# A missing `Test::*` module is silently tolerated at `use` time

Split out from `todo/tickets/bundle-json-tiny-instead-of-emulating.md`
(resolved 2026-08-14, `news/2026-08/bundle-json-tiny-battery.md`) — the JSON
bundling work in that ticket is done; this leftover wart is unrelated and
still open.

## Root cause

`src/runtime/runtime_module.rs`, `use_module_with_tags_inner`, the
`module.starts_with("Test::") && !self.require_propagates_missing_module`
branch: a `use Test::Foo` for a module that does not exist anywhere on the
search path is treated as a no-op instead of a load error, as long as it is
not reached through `require` (which does propagate
`X::CompUnit::UnsatisfiedDependency`).

```rust
} else if module.starts_with("Test::") && !self.require_propagates_missing_module {
    // Load Test:: submodules from source as regular modules.
    // Parse errors should propagate like other `use` failures.
    // Missing helper modules remain non-fatal for compatibility —
    // except under `require`, whose whole contract is that a missing
    // module is a catchable X::CompUnit::UnsatisfiedDependency
    // (HTTP::UserAgent's `t/001-meta` skips itself that way).
    match self.load_module(module) {
        Ok(()) => Ok(()),
        Err(err) if err.is_unsatisfied_dependency() => Ok(()),
        Err(err) => Err(err),
    }
}
```

## Affected files

- `src/runtime/runtime_module.rs` (the branch above)
- Observable effect: `modules/HTTP-UserAgent/`'s `070-ua-simple.rakutest`
  passes with no `IO::Capture::Simple` present anywhere in the bundle or
  search path — its body is behind a `NETWORK_TESTING` guard, so the
  unresolved `use Test::IO::Capture` never actually executes and the silent
  tolerance never gets exercised by that particular test.

## Why it is worth tracking (but not urgent)

The blanket `Test::*` leniency exists for real compatibility reasons (roast
and bundled suites `use` test-only helper modules mutsu does not vendor), so
it cannot simply be deleted. But it means a genuinely missing `Test::*`
dependency fails silently instead of surfacing as a clear "this module isn't
available" error, which can mask a real gap (a test that *should* have run
its assertions quietly runs zero of them). A narrower fix would need to:

- distinguish "this is a known-absent test helper we deliberately don't
  vendor" from "this is a typo / genuinely missing dependency", or
- at minimum warn (not silently no-op) when a `Test::*` `use` resolves to
  nothing, so a CI log at least shows the gap.

## Repro

```raku
use Test::ThisModuleDoesNotExistAnywhere;
use Test;
ok 1, "this line still runs — no error, no warning, for the missing use above";
done-testing;
```

Runs to completion with no error/warning under mutsu; a construct-and-observe
check confirms `require Test::ThisModuleDoesNotExistAnywhere` (not `use`) DOES
throw a catchable `X::CompUnit::UnsatisfiedDependency`, so the special-casing
is real and only affects the `use` form.
