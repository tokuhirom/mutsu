# `$*RAKU` is a `Raku`, not the pre-rename `Perl`

`$*RAKU.^name` reported `Perl`, `$*RAKU.put` printed `Perl()`, and plain
`say $*RAKU` printed `Raku (6.d)` — three spellings of the same object that did
not agree with each other, two of them still carrying the pre-Perl-6-to-Raku
name.

## Two independent root causes

**1. The compiler-identity type was registered only as `Perl`.** `runtime_init`
registered a single native class named `Perl`, and `make_perl_instance` built
its instance with that symbol, so `.^name` reported the old name. rakudo keeps
both names — `Raku.^name` is `Raku`, `Perl.^name` is `Perl`, and `$*PERL` still
answers with a `Raku` — so the fix is not a rename but making `Raku` the real
type. Both names are now registered from one loop over a shared native-method
table, `native_perl` dispatches for either, and `make_perl_instance` builds the
`Raku` one.

**2. `.put` never went through string context.** This turned out to be a
general bug, not a `$*RAKU` quirk: `dispatch_put` stringified its invocant with
the raw `Value::to_string_value()`, while its sibling `dispatch_print` used
`render_str_value`, which honors a user-defined or native `.Str` method. So
`.put` printed `ClassName()` for *any* object with a custom `.Str`:

```raku
class C { method Str { "custom" } }
C.new.print;   # custom
C.new.put;     # mutsu printed "C()", rakudo prints "custom"
```

`put` is `print` plus a newline, so it now uses the same renderer.

## Also fixed along the way

`$*RAKU.compiler.version` was a hardcoded `v0.1.0` while `.id` was derived from
`CARGO_PKG_VERSION` — the same build reported two different versions
(`v0.1.0` vs `mutsu-0.22.0`). Both the compiler's `version` and its `release`
now read the real crate version, and the compiler's name/auth are shared
constants so `.name`, `.id` and `.verbose-config` cannot drift apart.

Pin: `t/eval-compunit-introspection.t` asserts `.^name`, `.Str` and `.gist`
agree, and checks the `.put` string-context fix through a subprocess (so it
tests real stdout, not a mock). It passes verbatim under `raku`.
