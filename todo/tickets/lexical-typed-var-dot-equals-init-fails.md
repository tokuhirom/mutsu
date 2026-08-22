# `my Type:D $var .= new;` (lexical typed declaration with `.=` initializer) fails when the type carries a definedness smiley

Discovered while evaluating `Log::Dispatch` (a runner-up candidate to `Log::Async`, which is
already the chosen logging battery, for possible bundling). `Log::Dispatch.rakumod` fails to even
load in mutsu because its very first module-level statement is:

```raku
my Lock:D $reg-lock .= new;
```

## Root cause

`handle_method_call_assign` in `src/parser/stmt/decl/my_decl_assign.rs` builds the `.=` initializer's
call target from the declared type constraint string verbatim, without stripping a trailing
definedness smiley (`:D` / `:U` / `:_`):

```rust
let target_expr = match &s.type_constraint {
    Some(c) if s.name.starts_with('@') => { ... }
    Some(c) if s.name.starts_with('%') => Expr::BareWord(format!("Hash[{c}]")),
    Some(c) => Expr::BareWord(c.clone()),   // <-- line ~654: `c` still contains ":D"/":U"
    None => ...
};
let expr = Expr::MethodCall {
    target: Box::new(target_expr),
    name: Symbol::intern(&method_name),   // "new"
    ...
};
```

For `my Lock:D $reg-lock .= new;`, `s.type_constraint` is the string `"Lock:D"` (the smiley is
part of the type-constraint text as captured by the parser, and is never stripped for this
particular use). The compiled call becomes effectively `Lock:D.new`, i.e. a method-call target of
`Expr::BareWord("Lock:D")`. At runtime this resolves the *literal* bareword `"Lock:D"` as a type
name (rather than resolving `Lock` and treating `:D` purely as a definedness constraint the way
every other typed-variable path does), so method dispatch for `new` fails against a type object
whose display name is `Lock:D`:

```
X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on Lock:D
```

This reproduces for **any** type (built-in like `Lock`/`Int`, or a user-defined `class`), for both
`:D` and `:U` smileys, and for both `my` and `our` declarations. It does NOT reproduce for:
- `my Type:D $x = Type.new;` (explicit assignment, no `.=` desugaring) — works correctly.
- `has Type:D $!attr .= new;` (attribute declaration inside a class) — works correctly, so the
  `has`-decl path (`src/parser/stmt/decl/has_decl.rs`) must already strip the smiley before
  building its own `.=` target, unlike `my_decl_assign.rs`.

## Affected files

- `src/parser/stmt/decl/my_decl_assign.rs` — `handle_method_call_assign`, the `Some(c) =>
  Expr::BareWord(c.clone())` arm (~line 654) is the exact site; it needs to strip a trailing
  `:D`/`:U`/`:_` (and any parametrization after it, mirroring however `has_decl.rs` or
  `parse_type_constraint_expr` already do this elsewhere) before building the bareword target.
- Likely worth cross-checking the sibling `@`/`%` arms just above it (`Array[{c}]`/`Hash[{c}]`)
  for the same issue with `my Int:D @x .= new;`-style declarations, though this was not separately
  verified.

## Repro

Verified against `target/release/mutsu` built from this repo at commit `5ca0dc45b` (2026-08-22).

```raku
my Lock:D $reg-lock .= new;
say $reg-lock;
```

```
$ raku tmp/lockrepro.raku
Lock.new
$ target/release/mutsu tmp/lockrepro.raku
X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on Lock:D
  in block <unit> at tmp/lockrepro.raku line 1
```

Confirms the pattern generalizes beyond `Lock`:

```raku
my Int:D $x .= new;   # -> "Unknown method value dispatch (fallback disabled): new on Int:D"
class Foo { has $.x = 1; }
my Foo:D $f .= new;   # -> "Unknown method value dispatch (fallback disabled): new on Foo:D"
our Lock:D $x .= new; # -> same error (also affects `our`, not just `my`)
my Lock:U $x .= new;  # -> "... new on Lock:U" (also affects `:U`, not just `:D`)
```

Contrast with the working `has`-attribute case (same idiom, different declarator):

```raku
class Foo {
    has Lock:D $!lock .= new;
    method go { $!lock.protect: { say "hi" } }
}
Foo.new.go;   # prints "hi" correctly in mutsu
```

## Why this is a separate ticket

Unrelated to `Log::Async` (the chosen logging battery). It surfaced purely from probing
`Log::Dispatch` as a runner-up candidate during the logging-module bundling evaluation, where it
is the very first thing that breaks (`Log::Dispatch.rakumod` line 7, before any of the module's
actual logging functionality is reachable). A sibling ticket,
[`unit-monitor-declarator-not-supported.md`](unit-monitor-declarator-not-supported.md), documents a second, independent bug
(`Terminal::ANSI`'s transitive `unit monitor Name;` declaration) that `Log::Dispatch` also hits
once this one is worked around. `Log::Dispatch` itself was not pursued further once both were
found, since fixing either is out of scope for this evaluation task.
