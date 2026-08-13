# ADR-0019 E11 slice 5: REPL last-value gist display routes through the resolver

The REPL's last-value display (`repl_core.rs::process_line`) called
`native_method_0arg(&value, "gist")` directly, falling back to
`value.to_string_value()` on a miss. That native-only probe never sees a
user-defined `.gist` method on an `Instance`, so a class overriding `.gist`
displayed as a generic stringification instead of its own representation —
this was the last of the four sites deferred from ADR-0019's E11 slice 1/2
inventory (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`).

Routed the display through `Interpreter::call_method_with_values(value, "gist",
vec![])`, keeping the exact same fallback shape: any error (no `gist` handler
recognized) still falls back to `to_string_value()`.

Verified against real `raku`'s REPL: `class Foo { has $.x; method gist {
"Foo<{$!x}>" } }; Foo.new(x=>42)` answers `Foo<42>` in both. New pin:
`test_user_defined_gist_wins_in_repl_display` in `repl_core.rs`'s test module.
`cargo build`/`clippy -D warnings`/`fmt` clean; full local `make test` (3137
files) green.

This closes every site in ADR-0019 E11's original deferred-sites inventory;
the grep-based completion criterion (no caller of `native_method_{0,1,2}arg`
outside the resolver's two canonical entry points, `builtins/` internal
recursion, and `#[cfg(test)]`) can be re-checked next.
