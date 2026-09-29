# ADR-0077's argument move measured, and its last two slices retired

ADR-0077 left two pieces open: letting a leading-parameter callee use the
operand-stack slots as its locals (`locals_base = args_base`, so no argument is
moved), and folding the block-scope `outer_scope_locals` stack into the
contiguous locals stack (Slice 3). Both are now closed without code changes
(#9935).

The argument move was measured with instruction-level callgrind, mapping each
instruction back to the line of `call_compiled_function_positional_light_at` it
was inlined into. The whole argument hand-off costs 1.77% of `fib(22)` and
2.56% of `tak(14,7,0)` with the JIT on (1.05% / 1.68% with it off). That is the
gross ceiling, before the per-frame length a fused stack would add to every
locals access. The move itself is only 13 instructions per argument. The
largest part is the `stack.truncate(args_base)` after the bind, which
drop-scans `Nil` placeholders that own nothing. A local change can remove that,
so it is tracked separately as #10184.

Slice 3 turned out moot. Under shadow slots, the default, a block scope already
pushes an empty `Vec` (no allocation, no copy), so folding it in would save
only a 24-byte push/pop per block.
