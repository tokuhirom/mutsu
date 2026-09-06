# `$str ~~ $_` returns `Bool` instead of the `Match`

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/structures.rakudoc:220`)
and narrowed the same day against raku v2026.07.

## Repro

```raku
for (/a/,) { say ("ab" ~~ $_).raku }
# raku:  Match.new(:orig("ab"), :from(0), :pos(1))
# mutsu: Bool::True
```

The doc block it came from is the same defect one layer up:

```raku
my @regex-check = ( /<alnum>/, /<alpha>/, /<punct>/ );
say @regex-check.map: "33af" ~~ *;
# raku:  (｢3｣ alnum => ｢3｣  ｢a｣ alpha => ｢a｣  Nil)
# mutsu: (True True True)
```

## Narrowed — it is the topic, not the regex, the block or `.map`

Every other spelling of "smartmatch a string against a regex held in a variable"
answers with the `Match`:

| Program | raku | mutsu |
|---|---|---|
| `say ("ab" ~~ /a/).raku` | `Match` | `Match` |
| `my $r = /a/; say ("ab" ~~ $r).raku` | `Match` | `Match` |
| `sub f($x) { "ab" ~~ $x }; say f(/a/).raku` | `Match` | `Match` |
| `say ("ab" ~~ *)(/a/).raku` | `Match` | `Match` |
| `say (1,).map({ "ab" ~~ /a/ }).raku` | `Match` | `Match` |
| `my $r = /a/; say (1,).map({ "ab" ~~ $r }).raku` | `Match` | `Match` |
| `say (1,).map({ my $m = "ab" ~~ /a/; $m }).raku` | `Match` | `Match` |
| **`for (/a/,) { say ("ab" ~~ $_).raku }`** | `Match` | **`Bool::True`** |
| **`say (/a/,).map({ "ab" ~~ $_ }).raku`** | `Match` | **`Bool::True`** |

So `.map`, the block, the `WhateverCode` spelling and regex-in-a-variable are
all fine. The one condition that changes the answer is the right-hand side being
the **topic** `$_`.

Raku's rule is that `$x ~~ $y` returns whatever `$y.ACCEPTS($x)` returns — a
`Regex` returns its `Match`. Something on the `$_` path is instead asking for a
boolean.

## Where to look

The smartmatch implementation (`src/vm/vm_smart_match.rs`,
`src/vm/vm_smartmatch_ops.rs`) and, specifically, whatever distinguishes a
topic-valued right operand — a compile-time special case for `~~ $_`, or a
runtime path that reads the topic and coerces it. Confirm with a `rust-gdb`
breakpoint on the arm that produces the `Bool` rather than guessing which of the
two it is.

## Neighbourhood to check when fixing

`$_ ~~ $_`; the topic holding a `Junction`, a type object, a `Callable`, a
`Range`, a `Set` or a plain `Str` (each has its own `ACCEPTS` return value, and
only some of them are `Bool` in raku); `!~~` against the topic; `when $_ { }`;
`given`/`for` topics vs a `-> $_` block parameter; and `$/` being set correctly
after the match in every one of those (a `Match`-returning smartmatch must also
populate `$/`).
