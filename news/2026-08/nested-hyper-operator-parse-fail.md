# A hyper operator may wrap another hyper operator

`Language/operators.rakudoc:567` shows a hyper operator whose base operator is
itself written as a hyper operator, used to broadcast an element-wise binary
operation across two nested-tuple lists:

```raku
my $neighbours = ((-1, 0), (0, -1), (0, 1), (1, 0));
my $p = (2, 3);
say $neighbours »>>+<<» ($p, *);   # ((1 3) (2 2) (2 4) (3 3))
```

mutsu rejected this outright with `Confused. expected expression after hyper
operator`. The doc-diff harness filed it as
`todo/tickets/nested-hyper-operator-parse-fail.md`.

## Root cause

`parse_hyper_op` (`src/parser/expr/precedence_meta_ops/hyper_concat.rs`)
consumed the *left* delimiter and then scanned the next few bytes for the first
closing marker, taking everything in between as the base operator. For
`»>>+<<»` the first closer it found was the inner `<<`, so the base operator
came out as the bogus string `>>+` and the remaining `»` was left dangling —
hence the "expected expression after hyper operator" from the operand parser.

## What raku actually does, and the fix

rakudo represents the construct as a `RakuAST::MetaInfix::Hyper` whose `infix`
is another `RakuAST::MetaInfix::Hyper`, but the nesting turns out to be
**semantically inert**: hyper operators already descend into nested structures,
and only the OUTERMOST delimiter pair's dwim flags govern dimension mismatches.
That was established against raku before writing any code:

| expression | raku |
|---|---|
| `((1,2),(3,4,5)) »>>+<<» ((10,20),(30,40))` | `((11 22) (33 44 35))` — same as `»+»` |
| `((1,2),(3,4)) »>>+<<« ((10,20),(30,40,50))` | `X::HyperOp::NonDWIM` — same as `»+«` |
| `((1,2),(3,4)) «>>+<<« ((10,20),(30,40,50))` | `((11 22) (33 44 53))` — same as `«+«` |

So the parser now unwraps a nested spelling recursively — when the text right
after the outer opening delimiter is itself a hyper opening delimiter, the inner
hyper is parsed as a unit, its base operator is taken, and the closing marker
that follows it supplies the outer `dwim_right`. The outer pair's flags are
kept; the inner pair's are discarded, exactly as rakudo's semantics require.
A fully-ASCII outer pair (`<<<<+>>>>`) stays a parse error, as it is in rakudo
too ("Missing << or >>") — the spelling is genuinely ambiguous.

A side effect of the fix is that the base operator's precedence is now read
correctly for a nested spelling (`hyper_op_prec(">>+")` used to fall through to
`Other`; it is `Additive` now).

The four right-delimiter markers were also lifted into one
`HYPER_RIGHT_MARKERS` constant shared by the nested-unwrap and the candidate
scan, so the two cannot drift apart.

Pinned by `t/parser-expression-gaps.t`.
