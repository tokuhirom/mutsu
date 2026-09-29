# A parameter's `where`, default and shape are compiled once, not on every call

Binding a parameter with a `where` clause, a non-literal default (`$x = $y + 1`) or a shape
dimension (`@a[$n]`) used to hand the expression's AST to the binder, which compiled it again on
every call (`eval_block_value(&[Stmt::Expr(expr.clone())])`). A `where * > 0` paid for two
compiles per call: one for the clause, and one for the WhateverCode body, whose parse site was
new each time, so the carrier cache never hit.

The compiler now compiles these expressions once, when it compiles the routine or closure that
owns the signature. It stores them in a slot on the `ParamDef`, and every copy of that parse node
shares the slot. Every binder path runs the stored chunk: ordinary subs and methods, closures,
multi-candidate selection, sub-signatures and shape checks. [ADR-0133](../../docs/adr/0133-no-per-call-ast-compile-at-runtime.md)
(Proposed) makes "the runtime does not compile AST per call" the rule. It lists the remaining
per-call sites, each filed as its own issue: WhateverCode subscripts (#10118), sequence endpoints
(#10119), `.subst` closures (#10120), and regex `<{ }>` / `** {n}` / `:my` (#10121).

Three changes close the rest of the gap:

- `$x ~~ $code` and a subset predicate that evaluates to a closure now call the closure the way
  the VM calls any code value (`vm_call_on_value`). They no longer go through `call_sub_value`'s
  by-name env carrier.
- A one-argument WhateverCode predicate (`where * < 100`, and `subset … where * < 100`) runs its
  body with `$_` bound. No closure is built or called per check.
- A subset's registry entry is shared (`Arc`), so a type check no longer deep-clones it,
  predicate AST included.

The #10107 repro is a multi whose candidates select on a subset and on a `where` clause. It went
from 178.7k to 56.1k instructions per call (callgrind, the 3000-iteration loop minus `^0`). What
remains is the value-dependent dispatch machinery itself, which is still open under #10107.
