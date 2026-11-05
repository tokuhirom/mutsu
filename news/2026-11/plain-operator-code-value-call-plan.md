# A plain builtin operator called through a code value skips the call ladder

`my $c = &[cmp]; $c(1, 2)` used to walk the whole routine-value ladder (name
resolution, base-name and proto gates, the native-function table) and then
`call_infix_routine_in`'s prologue for hyper, assignment, set, sequence and
short-circuit operators before reaching the operator body. Now a two-operand call
of a plain operator (`cmp`, `<=>`, the numeric and string comparisons, `+ - * / % **`,
`~`, `===`, `before`/`after`, `min`/`max`) with no user routine, `proto` or Junction
operand goes from the routine value straight to the per-pair body
(`infix_pair`), whose override probes use keys interned once per operator.
Lexical `&infix:<op>` callables, block-scoped imports and user candidates still win.

Profiling build, warm, slope 100 -> 400 iterations of
`$s += $c(1, 2) eq "Less" ?? 1 !! 0`: 16.9k -> 12.0k Ir per iteration. Against the
inline `1 cmp 2` loop (8.8k), the cost of the call through the code value is now
3.2k Ir (was 8.1k after #15906, 13.4k before).
