# A streamed `.map(* + 1)` computes each element with the operator

`for @big.map(*+1) { ... }` runs the callback one element per pull (#9936). #10233 moved the
loop-invariant setup out of the pull but each pull still paid an env swap
(`enter_inline_loop_env`/`leave_inline_loop_env`, ~1.8k Ir), the bind and `run_reuse` for a body
that is one `Add` opcode: streamed was 1.55x the eager `.eager` form (#10187).

A prefix pull of a `.map` whose callback is `$_ OP constant` over an Int (the shape
`pure_int_topic_op` recognises, also used by the lazy-pipe path) now reads the source element by
element and applies the operator itself, with no env swap and no loop body. It reads the live
source at the moment of each pull, so a write between pulls is still seen; a non-Int element or an
i64 overflow hands over to the regular loop, which promotes to a BigInt as before. A map over a
real Array (`MapRw`) takes the same path, as `$_ OP constant` never writes `$_`.

callgrind (5000 elements, profiling build, empty program 7.36M Ir): streamed
`for @big.map(*+1) { $n += $_ }` 64.8M -> 29.1M Ir, against 46.4M for the `.eager` form, so the
streamed form now costs 0.56x the eager one per element.
