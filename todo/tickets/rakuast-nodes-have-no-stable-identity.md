# A RakuAST node is not `===` (nor `eqv`) to itself

`===` and `eqv` both answer `False` when both operands are literally the same
RakuAST node object. Raku guarantees `$x === $x` for any object.

```raku
use experimental :rakuast;
my $t = RakuAST::ParameterTarget::Var.new(name => '$x');
my $p = RakuAST::Parameter.new(target => $t);
my $q = $p;
say $p === $q;   # raku: True    mutsu: False
say $p eqv $q;   # raku: True    mutsu: False
```

## Why it matters beyond introspection

`===` is used as a *guard* inside the compiler-emitted `is rw` call-argument
writeback (`emit_index_rw_writebacks`, `src/compiler/helpers_call_args.rs`): the
writeback is skipped when the argument temp is still identical to the snapshot
taken before the call. On a value with no stable identity that guard fails open,
so the writeback runs and tries to assign back through the argument expression.
For `is $signature.parameters[0], $parameter, '...'` that meant an attempted
`$signature.parameters[0] = ...`, which died with

```
X::Assignment::RO: cannot assign through .parameters on non-instance
```

That path is currently dead for the `ExecCallPairs` dispatch shape (see
`news/2026-08/return-rw-produces-first-class-containers.md`), so the failure is
not reachable today — but any future work that gives that shape a writeback emit
point, or that widens the writeback machinery (ADR-0059 Slice 3), will hit it
again. The `eqv` fallback guard is no help: it is `False` here too.

## Root cause (unverified — start here)

`===` compares `.WHICH`. A RakuAST node is a `ValueView::RakuAst(node)`, a
distinct `Value` variant from `Instance` (see `value_type_name`'s
`ValueView::RakuAst(node) => node.class.printed_name()` arm in
`src/runtime/utils/type_misc.rs`), so it presumably has no `id`-backed `WHICH`
the way an `Instance` does, and falls through to a structural/`Nil` comparison
that never matches. Confirm by breaking on the `WHICH` dispatch in
`src/builtins/methods_0arg/dispatch_core_coerce.rs` for a RakuAst operand.

`eqv` on RakuAST nodes is a separate (and probably deeper) question: Rakudo's
`eqv` on two structurally identical but distinct nodes is `True`, which needs a
structural walk of the node's children. Identity (`===`) is the smaller, more
urgent half and can be fixed on its own.

## Affected files

- `src/value/types_eqv.rs`, `src/runtime/utils/compare.rs` — the `===`/`eqv`
  implementations.
- `src/builtins/methods_0arg/dispatch_core_coerce.rs` — the `WHICH` dispatch.
- `src/value/rakuast*` — where a node's identity would have to live.
