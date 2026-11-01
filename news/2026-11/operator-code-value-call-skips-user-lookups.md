# Calling a builtin operator through a code value skips the user-routine lookups

`my $c = &[cmp]; $c(1, 2)` (and `&infix:<+>(...)`, `.sort(&[<=>])`, `Array::Sorted::Util`'s
`:&cmp = &[cmp]`) reaches the operator by name through `call_function_compiled_first`, which
looked for a compiled user routine, then a resolved user def, and only then reached the native
operator handler. For a name no routine carries, both lookups were misses that cost ~5k Ir
(`find_compiled_function`'s type signature and memo probes, `resolve_function_with_types`).

The ladder now asks the base-name negative gate (`fn_base_name_registered_sym`, plus the
candidate-less-proto map) once and skips both lookups when no routine carries the name; a user
candidate, a module-imported candidate and a proto keep their old path. `call_infix_routine_in`
also stopped stringifying every operand to test for the `method` parser marker and building its
`infix:<op>` name with `format!`.

Measured with callgrind on a profiling build (slope between 100 and 300 iterations, warm), the
loop `$s += $c(1, 2) eq "Less" ?? 1 !! 0`: 22.2k -> 16.9k Ir per iteration; the same loop with the
inline `1 cmp 2` is 8.8k, so the cost of calling through the code value fell from 13.4k to 8.1k.
The issue's close condition (<= 6k for the call) is not met: what is left is `call_infix_routine_in`'s
own prologue and the per-pair user-override probes (#15847).
