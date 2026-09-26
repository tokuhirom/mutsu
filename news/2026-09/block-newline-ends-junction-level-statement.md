# A line-ending `}` now ends the statement before a leading `&`, `|` or `^`

Raku ends a statement at a closing brace that ends its line, so the next line
starts a new statement even when it opens with a character that could also be
an infix operator. mutsu already applied this rule at the additive,
multiplicative, concatenation and set-operator levels, but not at the junction
level. The block was therefore joined to the next line as an `&`, `|` or `^`
junction:

```raku
my package EXPORT::override {
    &OUR::override-user-timezone := sub override-user-timezone(Str $tz) { ... }
    &OUR::clear-user-timezone-override := sub clear-user-timezone-override { ... }
}
```

The first bind read on into `& OUR::clear-...` and then choked on the second
`:=`, reported only as the generic "Confused. expected statement" dump. Both
User::Timezone and UserTimezone contain this block, so neither could load its
own module (#9552, one of the parse gaps split out of #7988).

`junctive_expr_mode` now asks the same `block_newline_terminates` check the
tighter levels use before it looks for a junction operator. A junction written
on one line (`sub { 7 }() & 8`) is unaffected, and `|(...)` at the start of
the next line now parses as the prefix slip it is in rakudo.

The looser levels (`==`, `eq`, `~~`, `..`, `&&`, `//`, `and`) still continue
across a line-ending `}`, where rakudo instead reports a term-position error.
That is a separate gap and needs no change to programs rakudo accepts.

Pinned by `t/control/block-newline-ends-junction-statement.t`.
