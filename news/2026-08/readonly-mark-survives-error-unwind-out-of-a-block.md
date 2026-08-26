# A declaration now resets its name's readonly state, so a `:=`-to-literal marking can no longer poison a later same-named variable

`try { my $zz := 42; $zz = 23 }; my $y = 1; my $zz := $y; $zz = 23; say $y`
printed `23` under `raku` and died with "Cannot assign to an immutable value"
under mutsu. The ticket that recorded this framed it as an *error unwind*
leaking a marking out of a bare block.

## The measured root cause is different — and larger

The unwind was incidental. Re-measured on current `main`, the marking leaked out
of **every** inline body, throwing or not:

| body | leaked? |
| --- | --- |
| `try { my $zz := 42 }` | yes |
| `if 1 { my $zz := 42 }` | yes |
| `for 1..1 { my $zz := 42 }` | yes |
| `while … { my $zz := 42 }` | yes |
| `{ my $zz := 42 }` (bare block) | no |
| `sub s1 { my $zz := 42 }` | no |

Only the two shapes that open a *scope* of their own cleaned up: a bare block
(`OpCode::BlockScope`, whose exit already unmarks every `block_declared` name —
`src/vm/vm_misc_scope.rs`) and a routine frame (`pop_call_frame` →
`exit_readonly_frame`). `if` / `for` / `while` / `try` bodies compile to plain
instruction ranges with no readonly scope at all — an `if` branch that declares
a single `my` does not even emit a `BlockLocalScope`. So the ticket's suggested
fixes — "open a readonly frame for every block" or "scope the marking to the
declaring block" — would each have had to be threaded through several unrelated
body runners, and would still have missed the ranges that have no scope object.

The real invariant that was broken is simpler and does not depend on block
structure at all: **a `my` declaration re-establishes its name's readonly state
from scratch.** `readonly_vars` is keyed by the bare name, so a *fresh*
declaration must not inherit whatever an earlier binding of that name left
behind. The declaration path in `src/vm/vm_var_assign_set_local.rs` already did
exactly that — but excluded `:=` binds, because for a literal bind the marking
is applied by an op emitted *before* the store, so unmarking at the store would
have erased the declaration's own marking.

## The fix

Reorder, then drop the exclusion:

- `src/vm/vm_var_assign_set_local.rs` — the "scalar `:=` to a plain immutable
  value" arm no longer marks in place. It computes `bind_marks_immutable` and
  the marking is applied *after* the `is_vardecl` bookkeeping block, which now
  unmarks unconditionally rather than skipping `is_bind`/`scalar_bind`.
- `src/parser/stmt/decl/my_decl_assign.rs` — a `%h := …` bind emits its
  `Stmt::MarkReadonly(ImmutableValue)` after the declaration instead of before
  it (with a trailing re-read of the bound hash so the `SyntheticBlock` still
  yields the bound value in expression position, mirroring the array branch).
  The scalar-literal form keeps its pre-declaration `MarkReadonly`: it is erased
  by the declaration and re-applied by the store's own `bind_marks_immutable`
  arm — which covers exactly the literal kinds
  `scalar_binding_rhs_is_readonly` accepts — and moving it would have made
  `my $x := 5` evaluate to `Nil` instead of `5`, since a `SyntheticBlock` yields
  its last statement's value.

Everything that was immutable stays immutable: `my $x := 5; $x = 6`,
`my %m := mix <a b>; %m = (c=>1)`, `my @b := (1,2,3); @b[0] = 9`, and
`my constant PI = 3.14; PI = 5` all still throw the same exception classes as
`raku`.

Pinned by `t/itemization-and-readonly.t` (which passes under real `raku` too).
