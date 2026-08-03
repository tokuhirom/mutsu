# Two compile-time diagnoses roast asks for by class: chained ranges and `unless`+`else`

Continuing the exception-class residue under the real `Test` module
(`todo/tickets/vendor-real-test-module.md`): 29 of the 132 genuine failures lose
their first assertion to `right exception type (X::…)`. Two of those are
mechanisms rather than one-offs.

## The range operators are non-associative

`1..2..3` is `X::Syntax::NonAssociative` in rakudo, carrying both spellings in
`.left` / `.right` — the operators bind at one level and refuse to chain.
mutsu's `range_expr` builds exactly one range and returns, so the trailing
`..3` was simply left unconsumed and the statement failed with the parser's
generic "Confused", which named neither operator.

`range_expr` now checks for a following range operator after each of its four
forms and raises `non_associative_pair_error` — the constructor was already
there, used by the comparison chain (`1 <=> 2 leg 3`), and carries `.left` /
`.right` exactly as rakudo does. `..^` and `^..` report their own spelling.
`1...5` (the sequence operator) is deliberately not one of these, and neither is
a range inside a list or parentheses.

Freed: `roast/S03-operators/range.t` and `roast/S03-operators/precedence.t`.

## `unless` does not take `else`

rakudo rejects `unless 1 {} else {}` at *compile* time with
`X::Syntax::UnlessElse`, carrying the offending `keyword` — which
`roast/S04-statements/unless.t` matches on for all three of `else`, `elsif` and
`orwith`.

mutsu lowered it to a **runtime** `Stmt::Die` whose message merely *spelled* the
class name (`"X::Syntax::UnlessElse: unless does not allow 'else'"`), so it
arrived as a plain `X::AdHoc` with no `keyword` — and, worse, the rest of the
file still compiled and ran. It now goes through
`RuntimeError::unless_else`, a twin of the existing `without_else` that the
`without` branch has used all along, via `PError::from_typed`.

Freed: `roast/S04-statements/unless.t`.

## Attempted and reverted: `X::Syntax::DuplicatedPrefix`

The third mechanism in this batch did not survive measurement, and the record is
in `todo/tickets/duplicated-prefix-needs-metaop-aware-placement.md`. Raising the
diagnosis at the top of `prefix_expr` gets `~~1` / `^^5` right and passes
`make test`, but it breaks valid metaop code — `1 Z^^ 2` and `1 X^^ 2` are legal
in rakudo, and `1 Z?? 2 !! 3` is `X::Syntax::CannotMeta`
(`roast/S03-operators/ternary.t` test 28, which went red in `make roast`).
rakudo scans `Z^^` as **one infix token**, so the `^^` is never in term position;
mutsu's infix scanner consumes only `Z` and then hands `^^ 2` to `prefix_expr`.
The metaop scanner has to claim the whole sequence first — which is a bug on its
own, since `1 Z^^ 2` currently parses as `1 Z ^(^2)` and dies with
`X::Range::InvalidArg`.

Pin: `t/nonassoc-range-and-unless-else.t` — both diagnoses with their
attributes, the `without` twin alongside, and five assertions that plain ranges,
parenthesized ranges, the sequence operator and a plain `unless` are unaffected.
It passes under `raku` as well.
