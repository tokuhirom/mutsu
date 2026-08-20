# `5.`'s `X::Comp::Group` panic should be `X::Syntax::Malformed`, not `X::Comp::AdHoc`/"Confused"

## Status

Ready for direct implementation — small, well-scoped, single function, no design
needed. Split out of `news/2026-08/comp-group-multi-error-bundling-closeout.md`
(the deep finding it came from was verified stale and closed).

## Repro

```raku
my $e = try { EVAL '5.'; Nil } // $!;
say $e.^name;               # X::Comp::Group          (both)
say $e.sorrows[0].^name;    # X::Syntax::Number::IllegalDecimal (both)
say $e.panic.^name;         # raku: X::Syntax::Malformed  mutsu: X::Comp::AdHoc
say $e.panic.message;       # raku: Malformed postfix call  mutsu: Confused
say $e.message;
# raku : "Decimal point must be followed by digit\nMalformed postfix call"
# mutsu: "Decimal point must be followed by digit"
```

## Root cause

`illegal_decimal_point_error()` in `src/parser/expr/postfix/loop_.rs` builds the
group with a placeholder panic:

```rust
PError::comp_group(sorrow, false, "Confused", MSG.to_string())
```

`PError::comp_group` wraps whatever string it is handed in an `X::Comp::AdHoc`.
Rakudo's second complaint here is the real one it reaches after the decimal
literal fails: having rejected `5.` as a number, it retries the trailing `.` as
a method-call postfix, finds no method name, and panics with
`X::Syntax::Malformed` carrying `what => 'postfix call'`.

## Fix

Switch to `PError::comp_group_with_panic` with an `X::Syntax::Malformed`
exception (`what => "postfix call"`, `message => "Malformed postfix call"`), and
make the group's own `message` the two lines joined by a newline, matching
rakudo. `PError::malformed()` in `src/parser/parse_result.rs` already shows the
attribute shape `X::Syntax::Malformed` needs (`.what` is what roast matches on).

## Tests

`t/decimal-point-illegal-comp-group.t` already covers the class and the sorrow,
and its comment explicitly notes it only pins the sorrow text as a *prefix*
because rakudo's combined `.message` has a second line. Extend it to assert the
`.panic` class/`.what` and the full two-line `.message`. Also re-run
`roast/S32-exceptions/misc.t` (whitelisted, must stay green) —
its assertion only inspects `.sorrows[0]`, so it is unaffected either way.

## Non-goals

Do not touch the `when SomeUndeclaredType { ... }` case; that residual has a
different root cause and is tracked in
`todo/deep/when-undeclared-bareword-gobbles-block-needs-cross-file-type-index.md`.
