# `[before] 10, 9` agrees with `10 before 9`: one body per comparison operator

The same operator gave different answers depending on how it was spelled
(#9447):

| code | Rakudo | mutsu before |
|---|---|---|
| `10 before 9` | False | False |
| `[before] 10, 9` | False | **True** |
| `&infix:<before>(10, 9)` | False | **True** |
| `(2,) Zafter (10,)` | (False) | **(True)** |
| `Any eq ""` | True | True |
| `[eq] Any, ""` | True | **False** |
| `my $a = Any; ($a,) Zleg ("",)` | (Same) | **(More)** |

`a OP b` compiles to a dedicated opcode. `[OP]`, `»OP«`, `ZOP`/`XOP`
(through `eval_infix_leaf`) and `&infix:<OP>(...)` (through
`call_infix_routine`) fell back to `apply_reduction_op`, a pure table that
carried its own copy of each operator. The copy compared `before`/`after` as
strings, and it stringified an `Any` operand differently from the opcode's
coercion.

The fix gives each comparison-family operator one body, in the new
`src/vm/vm_operator_values.rs`: `str_cmp_values` (for `eq ne lt gt le ge`),
`before_after_values`, `identical_values` (for `===`/`!==`) and
`min_max_values`. `leg` keeps its existing `str_leg`. The opcodes pop their
operands and call these functions, which replaces five near-identical string
handlers. The metaop leaf and the routine form route the same operators here
through `comparison_family_values`, before splitting junctions, because these
bodies thread junctions themselves. A metaop hands over list elements
ref-preserving (so `=:=` can see the container), so the entry point reads
through a `VarRef`/container first; that was the `($a,) Zleg ("",)` case.
`apply_reduction_op`'s own `before`/`after` arm now orders by `cmp` too, for
the pure callers that remain.

This is the arrangement `num_eq_values` and the other numeric comparisons
already had (CLAUDE.md: a primitive has exactly one implementation).

Regression test: `t/lang/operators/comparison-operator-forms-parity.t` checks
the infix, routine, `Z`, `X`, hyper and reduction forms of twelve operators
over twelve operand pairs, plus Rakudo's answer for the cases above. Writing
it turned up a separate bug: metaops over a lexical code variable
(`Z[&op]`, `[[&op]]`) do not work at all. That is filed as #9464.
