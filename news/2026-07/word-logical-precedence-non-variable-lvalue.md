# Word-logical operators bind looser than a non-variable-lvalue assignment

The loose word-logicals (`and`, `or`, `xor`, `andthen`, `orelse`, `notandthen`)
are the loosest infix operators in Raku — looser than item assignment (`=`). A
plain-variable target already respected this, but every *other* lvalue shape —
an attribute accessor, an indexed array element, a hash key — swallowed the
operator into the assignment's right-hand side and stored the wrong value:

```raku
class C { has $.v is rw }
my $o = C.new; $o.v = 1 andthen 0;   say $o.v;   # was 0, now 1
my @a;         @a[0] = 8 andthen 0;  say @a[0];  # was 0, now 8
my %h;         %h<k> = 9 andthen 0;  say %h<k>;  # was 0, now 9
```

It stored the value of `(RHS op tail)`, so `and`/`andthen`/`notandthen` stored
the tail (or `Nil`); `or`/`orelse` only looked right by short-circuit accident.
The correct parse is `(<assignment>) andthen 0`: the assignment binds first, and
its assigned value drives the short-circuit, so the attribute/element keeps `1`
while the statement's value is `0`.

## Root cause

The defect was in the expression precedence chain, not (as first suspected) in a
statement-level fallback. `assign_not_expr_mode` parses a non-scalar lvalue's `=`
RHS with `parse_assignment_rhs_mode`, which used `ternary_mode` per comma
element. `ternary_mode` descends through `or_expr_no_assign_mode` /
`and_expr_no_assign_mode`, which *include* the loose word-logicals — so the RHS
absorbed the trailing `andthen 0`. (A bare scalar variable took a separate
`item_expr` RHS path that already stopped before the word-logicals, which is why
`$x = 7 andthen 0` was already correct.)

## Fix

`parse_assignment_rhs_mode` now parses each comma element with `list_infix_top`
instead of `ternary_mode`. That layer sits *just below* the loose word-logicals:
it still absorbs the list-infix operators (`Z`, `X`, their meta-ops, the sequence
`...`, `minmax`, feed) so `@a[^3] = 1, 2 ... 10` keeps folding the whole comma
list into one sequence, but it stops before `and`/`or`/`andthen`. The trailing
word-logical is left in the stream for the enclosing `and`/`or` chain, which
re-attaches it with the whole assignment as its left operand — evaluated exactly
once, no subscript re-evaluation.

The parenthesized / expression-context path for a subscripted lvalue
(`(@a[0] = 8 andthen 0)`) reaches `try_parse_assign_expr` instead, whose indexed
`=` branch parsed the RHS with the word-logical-inclusive `expression()`. It now
parses the RHS with `expression_no_word_logical` and re-attaches the tail via a
new `wrap_trailing_word_logical_expr` helper (the `IndexAssign` expression is the
tail's left operand, since it yields the assigned value).

Found while driving `HTTP::UserAgent` against a live server: `get-response` ends
with `$response.content = $content andthen $response.content = $response.decoded-content(:$bin)`,
which stored the wrong thing and surfaced as *"Type check failed for return
value; expected Blob:D but got Any"* from `inflate-content`.

Pin: `t/word-logical-lvalue-precedence.t`.
