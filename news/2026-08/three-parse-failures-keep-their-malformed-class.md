# Three parse failures keep their `X::Syntax::Malformed` class

The `right exception type (X::Syntax::Malformed)` cluster was the largest named
group in the real-`Test` roast residue, three files deep. None of the three was
a construct mutsu failed to reject — it rejected all three. Each rejection was a
*soft* parse error, so the enclosing alternative backtracked and the failure came
out as the parser's generic "Confused." with no class at all.

| construct | raku says | reached mutsu as |
| --- | --- | --- |
| `my $x = ` | `Malformed initializer` | `X::Syntax::Confused` |
| `.::` | `Malformed class-qualified postfix call` | `X::Syntax::Confused` |
| `:7` / `:7\x[308]a` | `Malformed radix number` | `X::Syntax::Confused` |

All three are now fatal errors carrying a real `X::Syntax::Malformed` with the
`.what` the roast tests match on, built by a new shared
`PError::malformed(what)` — there were already three hand-rolled copies of that
five-line construction in the parser.

`roast/S04-statements/terminator.t` and `roast/S02-literals/pairs.t` pass under
`MUTSU_REAL_TEST=1`. `roast/S12-methods/qualified.t`'s assertion passes too; the
file moves on to an unrelated `Cannot dispatch to method me on Parent` in its
inheritance subtest.

## The `.::` half was two different code paths

`$x.::` already produced a Malformed — with mutsu's own wording ("Malformed
qualified method name"), now raku's. But `.::` at statement start is the *topic*
form and never reached that check, so it fell through every alternative. Both
paths raise the same error now.

## What the initializer fix had to learn: only when nothing was readable

The first version converted *every* RHS failure after `=` into "Malformed
initializer", and local `make roast` caught two regressions that `make test` did
not: `my @a = 1, => 2` must be `X::Syntax::InfixInTermPosition`
(`roast/S32-exceptions/misc2.t`) and
`my $foo = { given $bar { when Real { 1 } ... } }` must be plain
`X::Syntax::Confused` — both had produced the better diagnosis already and the
blanket conversion masked it. `roast/S03-operators/ternary.t` lost
`X::Syntax::ConditionalOperator::PrecedenceTooLoose` the same way.

So the conversion applies only when the RHS parse failed *without consuming
anything*, and never over an error that is already fatal or already carries a
classified exception. An initializer that parsed part of itself keeps its own
diagnosis, which is what rakudo reports too.

This is the second time in two days that a new diagnostic was only safe once it
was measured against the full roast suite rather than `make test` — see
`news/2026-08/metaop-doubled-infix-base.md`.

Pin: `t/malformed-syntax-classes.t`, which asserts both directions — the three
classes, and the two failures that must *not* be flattened into them.
