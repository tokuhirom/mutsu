# Sequence generator and endpoint closures run their compiled code

The sequence operator used to evaluate a closure endpoint (`1, 2, 4 ... * > 50`)
by binding its parameters into the environment by hand and evaluating the
closure's AST body — which compiled that body afresh for **every element
checked**. A closure generator (`1, 2, * + * ... *`) had its body re-compiled
once per sequence for the same manual-binding fast path. Both now go through
the VM's ordinary value-call path (`vm_call_on_value`), which runs the
closure's own `compiled_code`, so a sequence costs no `Compiler::compile` call
at all (#10119).

How many trailing elements each call receives is now the closure's Raku
`.count`, read once per sequence, instead of an ad-hoc reading of its
parameter names. That fixed two behaviours along the way:

- a multi-parameter endpoint is no longer called with `Nil` padding before
  enough elements exist (`1, 2, 3 ... -> $a, $b { ... }` used to see
  `(Nil, 1)` first; Rakudo starts at `(1, 2)`);
- a seedless generator with side effects (`my $i = 0; { ++$i } ... * > 3`)
  now leaves `$i` at 4 in the enclosing scope, as Rakudo does, rather than 0.

The hand-rolled environment merge (`install_sequence_closure_env`), the
persisted per-sequence closure env and the precompiled-body field of the lazy
closure-sequence state are gone with it.
