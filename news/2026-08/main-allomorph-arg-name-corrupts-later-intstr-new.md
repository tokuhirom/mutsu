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

## Two pre-existing bugs it flushed out

Binding real allomorphs into `MAIN` turned `roast/S06-other/main-usage.t` red,
which was correct of it — both causes were real:

**`val()` mis-read whitespace.** It trimmed before parsing, so every
all-whitespace string numified: `val(" ")` was `IntStr.new(0, " ")`. Rakudo
treats whitespace *around* a number as insignificant (`val(" 42 ")` is
`IntStr.new(42, " 42 ")`) but a non-empty all-whitespace string as plain text;
only the genuinely EMPTY string numifies, to `IntStr.new(0, "")`. `builtin_val`
now returns the string unchanged when it is non-empty but trims to empty.

**`.ord`/`.ords` on an allomorph read its number, not its string.**
`IntStr.new(0, "zero").ords` was `(48,)` — the codepoint of `"0"` — because
those two were missing from the allomorph string-method list in
`builtins/methods_0arg/mod.rs`, so the generic mixin delegation handed them the
inner `Int`. They read characters exactly as `comb`/`chars` do, and are now in
that list. (`.uc`, `.trim`, `.flip`, `.comb`, `.chars`, `.NFC` and friends were
already correct; `.wordcase` remains divergent for a reason that needs its own
decision — split off to
[`todo/tickets/allomorph-wordcase-reads-the-numeric-part.md`](../../todo/tickets/allomorph-wordcase-reads-the-numeric-part.md).)

## The ticket's secondary finding was already fixed elsewhere

The same doc example also noted `< 1/2>.^name` (a *space-padded*
angle-bracket-quoted `Rat` word) reporting plain `Rat` where rakudo says
`RatStr`. Re-verified while working this ticket: mutsu now agrees with rakudo on
all three of `< 1/2>` (`RatStr`), `<1/2>` (`Rat`) and `< 42>` (`IntStr`) — that
one was closed out by
[angle-bracket-quote-word-allomorph-whitespace](angle-bracket-quote-word-allomorph-whitespace.md).
