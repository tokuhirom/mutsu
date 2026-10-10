# Fixes found by RedX::HashedPassword: `:=` list binds under a modifier, `&*name`, `::CALLERS::<&x>`, `do` + `without`

Working RedX::HashedPassword's `t/020-basic.t` (it drives Red's `create`) exposed four independent gaps:

- `my ($a, @b) := RHS if COND` was lowered as a list *assignment*, wrapping an array element in a one-item array; it now binds, and the names are declared even when the condition is false. A statement modifier no longer opens a block-local scope for its body.
- An unbound `&*name` resolved to a placeholder routine, so `so &*NOPE` was true. Only the core dynamic routines (`&*chdir`, ...) keep that resolution.
- `::CALLERS::<$*x>` (leading `::`) parsed as a bareword instead of the `CALLERS::` pseudo-stash, and `CALLERS::<&routine>` never looked at caller scopes.
- A nested `if` inside `do { }` whose body held a `... without $x;` modifier lost its value.

The remaining failure of that test (caller `$a`/`$b` clobbered by a rejected multi candidate) is tracked in #12512.
