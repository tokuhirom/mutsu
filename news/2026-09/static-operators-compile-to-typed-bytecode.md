# Reduction, hyper and meta operators compile to typed bytecode

`[+]`, `[\!after]`, `>>+<<` and `@a Z+ @b` all have a shape the parser fixed
once. Until now the compiler flattened that shape back into a string, stored it
in the constant pool, and the VM re-derived it on every execution of the
opcode: allocate a `String` from the pooled constant, strip the `\` scan
marker, try the `!` negation marker behind a linear scan of an 80-entry table,
fold the Unicode operator aliases, strip the compiler's own `_sc_`
thunked-short-circuit marker, strip the statically decidable `R` prefixes —
and then `format!("infix:<{}>", op)` up to four separate times while resolving
the operator's associativity and its possible user declaration.

None of that is a runtime question. `src/compiled_operator.rs` now owns the
decode:

- `ReductionSpec::lower` turns the operator spelling into
  `{ scan, negate, shortcircuit, reverse, base }` once, at compile time, and
  `OpCode::Reduction`'s operand indexes a per-chunk `reduction_specs` table
  instead of the constant pool.
- `MetaKind` replaces the pooled `"reduce"` / `"R"` / `"X"` / `"Z"` string that
  `OpCode::MetaOp` (and its assignment / n-ary siblings) used to `match` as
  text; the base operator travels as an interned `Symbol`, as it does for
  `OpCode::HyperOp`.
- `infix_names` memoizes the `infix:<op>` / `&infix:<op>` / `&op` spellings an
  operator resolves through, keyed by the spelling rather than by an interned
  `Symbol` — interning the lookup key would itself be the per-execution work
  the memo exists to remove.

Dynamic semantics are untouched: a user-defined infix is still resolved at run
time, and a `[R&callable]` inner still has its `R` stripped by the VM, because
whether `&foo` names a callable is a runtime question the compiler cannot
settle. `ReductionSpec::lower` applies its markers in exactly the order the VM
used to, so the decoded shape is identical to what each execution used to
compute.

The change also collapses four byte-identical copies of the hyper-delimiter
stripper (`>>op<<` / `»op»`) — in the parser, in `runtime/utils/type_misc.rs`,
and twice in the VM — and two copies of the Unicode alias table into one each.

## The guardrail

A reduction, hyper or meta opcode no longer accepts a constant-pool index for
its operator, and `ReductionSpec`'s fields are decided only by
`ReductionSpec::lower`. So a new statically spelled operator path cannot be
added by emitting one more string for the VM to re-parse; it has to extend the
lowering first.

## What was measured, and what was not

`tests/static_operator_intern_budget.rs` pins the steady state: a looped `[+]`,
`>>+<<` and `Z+`, and a `[+]` fold over a growing list, each add **0.000**
`Symbol::intern` calls per execution and per element.

A user-defined infix reduction is a different story and worth recording
honestly. `[myop] 1, 2, 3` costs ~7.6 interns per fold step both before and
after this change, all of them inside `call_user_routine_direct`'s by-name
dispatch — which is [#7766](https://github.com/tokuhirom/mutsu/issues/7766)'s
subject, not this one. What this change owns there is that the operator's own
name is derived once rather than per execution; the remaining cost is the
routine call, and the test asserts only that a longer operand list adds call
cost and nothing else.

[#8997](https://github.com/tokuhirom/mutsu/issues/8997) stays open: its
`CompiledOperator` direction also asks for the builtin operators themselves to
become enum variants, so that the VM matches a typed operator rather than
comparing `base_op` against string literals. That is the larger half, and the
per-element leaf dispatch in `eval_reduction_operator_values` — which still
re-strips `[`, `R`, `Z` and the hyper delimiters for every element of a hyper
or zip — is the part with the most left on the table.
