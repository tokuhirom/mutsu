# A statement-leading bare block followed by a word infix now parses

`{ ... } or die`, `{ ... } and say`, `{ ... } eq $x` and the other built-in
word-form infix operators (`xor`, `andthen`, `orelse`, `notandthen`, `eq`,
`ne`, `lt`, `gt`, `le`, `ge`, `cmp`, `leg`, `coll`, `unicmp`, `eqv`, `min`,
`max`, `x`, `xx`, `before`, `after`, `gcd`, `lcm`, `o`, `ff`, `fff`) used to
fail to parse when they immediately followed a statement-leading bare block
on the same line: `block_stmt` (`src/parser/stmt/simple/control_stmts.rs`)
committed to a bare-block statement unconditionally, so the operator was
left for the next iteration of the statement list to parse as a bogus new
statement (`Undeclared routine: or used`).

The parser already had this lookahead for a *user-declared* custom infix
(`{ $x--; } zork 25;`), so it just needed the same treatment for the
built-in word forms — added as `BUILTIN_INFIX_WORDS`, checked by the renamed
`starts_with_infix_operand_marker`. An ordinary call name (`say`, `logit`)
is not in that list, so a bare block followed by one on the same line still
parses as two independent statements, matching the existing custom-infix
behavior.

Since a bare block used as a term is a (truthy) `Block` object that is never
run, `{ ... } or die` now matches rakudo exactly: the block body never
executes and `die` is never reached, because `or`'s left operand is already
true. Pinned by `t/control/bare-block-followed-by-word-infix.t`.

Closes #9781.
