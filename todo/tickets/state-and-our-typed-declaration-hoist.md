# `state Int $x` is not in effect at block start, and `our Int $x` should not compile

Follow-up to
`news/2026-09/typed-declaration-hoist-missing-in-most-block-forms.md`, which
wired the compiler's `hoist_typed_var_decls` pre-pass into every
block-compilation entry point. That pre-pass deliberately skips `state` and
`our` declarations (`is_state: false, is_our: false` in its match), so two
divergences from Rakudo survive it.

## 1. `state TYPE $x` is not visible before its declaration statement

```raku
my $x = 0;
{
    my $err = "NO-DIE";
    try { EVAL '$x = "abc"'; CATCH { default { $err = .^name } } };
    say $err;
    state Int $x;
}
```

Rakudo: `X::TypeCheck::Assignment`. mutsu: `NO-DIE`.

Raku's block-start declaration visibility applies to `state` exactly as it does
to `my`, so the constraint should be registered at block entry. The reason the
hoist excludes `state` is that a `state` variable's storage persists across
invocations of the block while the `SetVarType` op also *seeds* a Nil-valued
typed scalar with its type object — running that seeding on every entry could
reset a `state` variable that has already been initialized. Getting this right
needs the constraint registration split from the seeding, or a hoist variant
that registers the constraint only.

## 2. `our TYPE $x` is accepted, and Rakudo rejects it at compile time

```raku
our Int $x;
```

Rakudo: `===SORRY!=== Cannot put a type constraint on an 'our'-scoped variable`.
mutsu: compiles, and enforces the constraint at runtime.

This is a missing compile-time check in the declaration parser/compiler, not a
hoist question. It is small but touches every `our` declaration path, so it
wants its own change and its own `t/` pin.
