# The `.=` metaop keeps its read-modify-write origin through the parser

`$.attr .= meth` was the last `$.`-twigil read-modify-write that still refused.
mutsu threw `Cannot modify an immutable Str (a)`; raku answers the computed
value, leaves the attribute alone, and throws nothing.

```raku
class C { has Str $.s = "a"; method m() { $.s .= uc; say "attr=" ~ $!s } }
C.new.m;   # raku: attr=a     mutsu (before): Cannot modify an immutable Str (a)
```

## Why it happened

`$.attr` inside a method is `self.attr` **itemized**. A non-`rw` scalar accessor
hands back a bare value, so the itemization is a fresh throwaway `Scalar`, and a
read-modify-write assigns into that throwaway: the expression yields the computed
value, the attribute is unchanged, and nothing is thrown. Only the simple
`$.attr = v` is compiled without the itemize wrapper, so it alone hits the raw
accessor return and dies.

mutsu already implemented that split — the `AssignExpr` opcode carries a
`dot_twigil_rmw` flag and `check_dot_twigil_accessor_writable` answers "skip the
store" for it — and every `OP=` form already set it, because the parser preserves
those as `Expr::CompoundAssign` markers. `.=` did not: it lowered straight to a
plain `Expr::AssignExpr`, byte-identical to what the hand-written
`$.s = $.s.uc` produces. No AST shape could tell them apart, and raku answers
them differently, so the missing piece was a *carrier for the `.=` origin
through the parser*.

## The fix

Every `.=` lowering on a simple-variable lvalue now goes through one funnel,
`postfix::dot_assign::dot_assign_to_name`, which stamps the expansion with the
existing source-preserving compound-assignment marker
(`Expr::CompoundAssign { op: ".=" }`, built by `dotty_assign_marker`). The
compiler's `Expr::CompoundAssign` arm already sets `dot_twigil_rmw_assign` from
the marker's target, so no compiler or VM change was needed at all — the runtime
decision was one marker away, exactly as the ticket predicted.

Rakudo agrees `.=` is its own construct: `Q[my $a; $a .= uc].AST` is
`ApplyDottyInfix(left, DottyInfix::CallAssign, Call::Method)`, a node distinct
from the `MetaInfix::Assign` used for `+=`. mutsu's RakuAST converter does not
model those classes yet, so it unwraps a `.=` marker to its expansion (exactly
what the bare `AssignExpr` rendered before) with a TODO pointing at the real
shape.

Three routes were considered and two rejected on measurement, not on guesswork:

- Adding a field to `Expr::AssignExpr` — the "honest home", but 183 references
  and a diff that conflicts with every sibling PR.
- The ticket's warning that reusing `Expr::CompoundAssign` would "double-count
  placeholders" via `collect_ph_expr` / `collect_ph_expr_shallow` turned out to
  be **wrong**: both walkers sort and `dedup()` their output, so walking `rhs`
  and `expanded` separately is idempotent. That removed the only real objection
  to the marker route.

## Bonus: a source-text hack retired

`ternary.rs`'s `assign_operator_is_tight` carried its own TODO asking for exactly
this marker. `.=` binds at method-postfix (dotty-infix) precedence, far tighter
than `?? !!`, so it is the one assignment-like expression legal unparenthesized
inside a ternary branch — while `=` and `OP=` must be rejected with
`X::Syntax::ConditionalOperator::PrecedenceTooLoose`. Because both spellings
produced the same AST, the check had to re-scan the branch's **source text**. It
now reads the marker off the AST, and the four call sites pass the parsed
expression instead of a string slice.

## Control table (measured in both implementations)

| shape | raku | mutsu before | mutsu after |
| --- | --- | --- | --- |
| `$.s .= uc` (non-`rw` `Str`), statement | `attr=a` | dies `Cannot modify an immutable Str (a)` | `attr=a` |
| `$.s .= uc` (non-`rw` `Str`), expression | `expr=A attr=a` | dies | `expr=A attr=a` |
| `$.n .= succ` (non-`rw` `Int`) | `expr=6 attr=5` | dies | `expr=6 attr=5` |
| `$.s = $.s.uc` (non-`rw`) | dies `Cannot modify an immutable Str (a)` | dies (same wording) | unchanged |
| `$!s .= uc` (private slot) | `expr=A attr=A` | `expr=A attr=A` | unchanged |
| `$.s .= uc` with `is rw` | `expr=A attr=A` | `expr=A attr=A` | unchanged |
| `my $x .= uc` (plain lexical) | `expr=A var=A` | `expr=A var=A` | unchanged |
| `@.a .= map` / `%.h<k> .= uc` (non-`rw`) | writes through | writes through | unchanged |

Pinned by `t/dot-assign-metaop-rmw.t` (25 rows, green under raku), which also
covers the neighbouring `.=` spellings the marker now flows through: chained
`.=`, parenthesized/colon/quoted-name arguments, array and hash elements, whole
containers, the hyper form, the `my Int $x .= new` declaration, use as a call
argument, and both ternary-precedence answers.

## Deliberately still open

`self.s .= uc` dies in both, but mutsu words it `X::Assignment::RO: method 's'
is not rw` where raku uses the immutable-value wording. That is cosmetic, spans
four `is not rw` sites in `methods_mut_method_lvalue.rs` plus
`methods_mut_dispatch.rs`, and each must render the *current* attribute value —
its own slice.
