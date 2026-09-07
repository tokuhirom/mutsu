# A one- or zero-element reduction with a user operator now calls it

`[myop] 5` returned `5` where rakudo calls `infix:<myop>` with the single
element and lets its binder die ("Too few positionals passed; expected 2
arguments but got 1"), and `[[&f]]` with no operands did not reduce at all —
the parse fell back to reading it as the array literal `[f]`.

## The rule mutsu was missing

rakudo's reduce metaop short-circuits a one-element fold only for operators it
knows an **identity** for. Those are the builtins, and mutsu already got them
right: `[+] 5` is `5`, `[+]` is `0`, `[*]` is `1`, `[~] 5` is the Str `"5"`. A
user-supplied routine has no identity, so it is simply *called* with whatever
elements there are — which for a 2-ary routine is an arity error, and for a
1-ary one is a legitimate call: `sub one($a) { $a * 10 }; [[&one]] 5` is `50`.

`Interpreter::exec_reduction_op` (`src/vm/vm_misc_reduction_exec.rs`) answered
`list[0]` for every operator before consulting the callable, and answered an
empty fold from the identity table (falling back to a "no zero-arg meaning"
Failure) rather than calling the routine with no arguments at all.

The *scan* form is deliberately left alone: rakudo does not call the operator
there either, so `[\myop] 5` and `[\[&one]] 5` are both `(5)`.

## The `&[+]` exception

A callable can also merely *name* a builtin — `my &op = &[+]` — and rakudo
still answers `[[&op]]` with `0` and `[[&op]] 5` with `5`, because the metaop
keeps that operator's identity. So the callable is now unwrapped back into its
operator (`reduction_builtin_op_for_callable`, `src/vm/vm_misc_ops.rs`) before
any arity, associativity or identity decision is taken, which also makes the
associativity of `&[**]`-style bindings follow the real operator. A user
declaration of the same name still wins and is called as a user routine.

## The zero-operand parse

`reduction_op` (`src/parser/primary/misc/reduction.rs`) recognised a
zero-operand reduction only when the terminator followed the `]` immediately,
so `try { [myop] }` and `say ([myop] )` — a space before `}` or `)` — fell
through to the array-literal reading. Horizontal whitespace is now skipped
before that test. Newlines deliberately are not: the listop path below reads an
operand across a line break, and consuming one would change which construct a
following line belongs to.

Pinned by `t/reduction-user-op-arity.t`, whose 16 assertions pass unchanged
under rakudo.
