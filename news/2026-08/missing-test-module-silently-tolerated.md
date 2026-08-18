# A missing `Test::*` module now warns instead of silently no-op'ing

`use Test::Foo` for a module that does not exist anywhere on the search
path used to be a completely silent no-op (`src/runtime/runtime_module.rs`,
`use_module_with_tags_inner`'s `module.starts_with("Test::")` branch) — no
error, no warning, as long as the `use` wasn't reached through `require`
(which already propagates a catchable `X::CompUnit::UnsatisfiedDependency`).

That leniency is intentional and stays: roast and bundled suites `use`
test-only helper modules mutsu does not vendor, and real `raku` hard-errors
on a genuinely missing `use`d module, so this is mutsu-specific
compatibility scaffolding, not something to delete. But total silence could
mask a genuinely missing dependency (a typo, not a deliberately-unvendored
helper) behind a test file that quietly ran zero of its assertions.

## Fix

The `X::CompUnit::UnsatisfiedDependency` branch now also writes a stderr
note (`WARNING: could not find module Foo::Bar to use, ignoring`, via
`self.write_warn_to_stderr`, mutsu's existing output-sink-aware warning
writer) before returning `Ok(())`. Still fully non-fatal — the rest of the
script keeps running, `require` is unaffected — but a CI log now shows the
gap instead of hiding it.

## Tests

`t/missing-test-module-use-warns.t` (new) — spawns a subprocess `use`ing a
nonexistent `Test::*` module, asserting the stderr note names the module,
the script still runs to completion (exit 0, later assertions still
execute), and `require` of the same nonexistent module is unaffected.

PR [#TBD](https://github.com/tokuhirom/mutsu/pull/TBD).
