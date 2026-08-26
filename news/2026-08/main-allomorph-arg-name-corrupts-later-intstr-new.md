# `MAIN` binds its command-line arguments through `val()`, so a numeric argument is an allomorph

The ticket reported this as order-dependent global corruption:

```raku
@*ARGS = "42";
sub MAIN($x) { say $x.^name; }
say IntStr.new(42, "42").^name;
# raku:  IntStr then IntStr
# mutsu: IntStr then Str
```

with the reading that `say $x.^name` inside `MAIN` left some memoized HOW /
mixin-overrides structure corrupted, degrading the *next* `IntStr.new(...)` to a
plain `Str`.

## Root cause — the causality was backwards

`MAIN` runs **after** the mainline, so the two output lines are in the opposite
order from the two statements. The `IntStr` line is the mainline's
`IntStr.new(42, "42").^name` (always correct, in isolation and here), and the
`Str` line is `MAIN`'s own `$x.^name`. Nothing was ever corrupted: **`MAIN`'s
argument simply was not an allomorph in the first place.**

Adding a `say "mainline"` before the `sub MAIN` declaration makes the ordering
plain, and a direct probe confirms it:

```raku
sub MAIN($pos, :$named) { say $pos.^name; say $named.^name }
# raku:  IntStr / RatStr
# mutsu: Str    / Str
```

Rakudo runs every command-line argument through `val()` before binding
(`Rakudo::Internals::PROCESS-ARGS`), so `42` reaches `MAIN` as an `IntStr`,
`3.5` as a `RatStr`, `1e3` as a `NumStr`, and anything non-numeric stays a plain
`Str`. That is what lets an untyped `MAIN` parameter be used as both a number
and a string. mutsu's `parse_cli_args` built every argument with `Value::str`.

## Fix

`parse_cli_args` now passes each positional argument, and each named option's
*value*, through the existing `val()` implementation. Bare flags (`--verbose`,
`--/verbose`, `--no-verbose`) are `Bool`s rather than text and are untouched, as
is the `numeric_suffix_as_value` short-option form, which already produced an
allomorph.

Pinned by `t/numeric-coercion-gaps.t`, which runs a `sub MAIN($pos, :$named)`
script in a subprocess under `$*EXECUTABLE` and asserts `IntStr`/`RatStr` plus
that the allomorph still behaves as a number.

## The ticket's secondary finding was already fixed elsewhere

The same doc example also noted `< 1/2>.^name` (a *space-padded*
angle-bracket-quoted `Rat` word) reporting plain `Rat` where rakudo says
`RatStr`. Re-verified while working this ticket: mutsu now agrees with rakudo on
all three of `< 1/2>` (`RatStr`), `<1/2>` (`Rat`) and `< 42>` (`IntStr`) — that
one was closed out by
[angle-bracket-quote-word-allomorph-whitespace](angle-bracket-quote-word-allomorph-whitespace.md).
