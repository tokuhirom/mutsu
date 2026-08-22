# A character class written with set subtraction terminates the declarative LTM prefix

`todo/deep/named-subrule-unbounded-quantifier-wrongly-gets-greedy-ltm-credit.md`
recorded that mutsu over-credited a candidate's declarative Longest-Token-Matching
prefix in this shape:

```raku
my regex catchall { <[\x1F..\xFF] - [;]>+ }
say "Foobar" ~~ / <catchall> | 'Foo' /;   # raku: Foo   mutsu: Foobar
```

Fixed as part of [ADR-0046](../../docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md)
Slice 4 — but **the root cause the ticket recorded was wrong**, and it is worth
saying why, because the wrong theory was expensive: it proposed a new
thread-local threaded through the quantifier-repetition loop so that an
unbounded quantifier reached *through a named subrule call* would stop at its
first repetition while the same quantifier at the top level stayed greedy.

## The actual rule

The named subrule is irrelevant — it is fully transparent, and the identical
divergence appears with the class written inline. The distinguishing factor is
the **character class's written structure**: a class built with set
*subtraction* cannot be encoded as a single NFA edge in Rakudo, so it becomes a
fate arc and terminates the declarative prefix. Every subtraction-free class
participates normally.

Validated against `raku` across the whole shape space (subject `"Foobar"`,
alternation second branch `'Foo'`):

| pattern | raku | terminates? |
|---|---|---|
| `<-[;]>+` | `Foobar` | no |
| `<[\x1F..\xFF]>+` | `Foobar` | no |
| `<[A..z]>+` | `Foobar` | no |
| `<[Fobar]>+` | `Foobar` | no |
| `<alpha>+`, `<+alpha>+` | `Foobar` | no |
| `\w+`, `.+` | `Foobar` | no |
| `<[\x1F..\xFF] - [;]>+` | `Foo` | **yes** |
| `<[A..z] - [;]>+` | `Foo` | **yes** |
| `<[Fobar] - [q]>+` | `Foo` | **yes** |
| `<+alpha - [q]>+` | `Foo` | **yes** |
| `<-[;] - [q]>+` | `Foo` | **yes** |
| `<[\x1F..\xFF] - [;]> \w*` | `Foo` | **yes** |

Two things the table settles. It is **not** about the quantifier: the last row
has a single unquantified subtracted class and still terminates. And it is
about the *written structure*, not the resulting character set: `<-[;] - [q]>`
denotes exactly the same set as `<-[;q]>`, yet only the first terminates.

## The fix

`ltm_atom_mode` (`src/runtime/regex/regex_ltm_rank.rs`) classifies a
`RegexAtom::CompositeClass` with a non-empty `negative` as
`LtmAtomMode::Terminate`. For the last row's shape to reach that arm,
`parse_bracket_char_class` (`src/runtime/regex_parse_charclass.rs`) stopped
collapsing a *multi-part* all-negative class (`<-[;] - [q]>`) into a plain
negated `CharClass`; it now keeps it as a `CompositeClass` with an empty
`positive` (which the matcher already reads as "any character"), so the
subtraction survives into ranking. A single-part `<-[;]>` is still a plain
`CharClass` — no subtraction was written.

## Why it became urgent

ADR-0046 Slice 4 made it decisive. While mechanism 3 (nested `<name>` proto
dispatch) did no declarative measurement at all, this over-credit was masked:
`t/grammar-body-my-lexical-scope.t`'s `token val:sym<other> { <gbmls-path> }`
and its array-interpolating sibling were both wrong in the same direction and
happened to rank in the right relative order. Once mechanism 3 started
measuring, the over-credit decided the outcome and the test went red. The test
had earlier been reshaped to route around this gap; it is now restored to its
strong `:rule<val>` form. Pinned by `t/regex-ltm-proto-dispatch.t` (the
"subtracted class" block, including the subrule-transparency controls).
