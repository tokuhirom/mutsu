# A warm `$o.m()` skips the opcode's receiver probes

The plain-method lane (#8880) remembers, per receiver class and method name, that
the pre-dispatch probe chain declined a call. Its gate sat after a stretch of
opcode-level receiver probes (Seq/lazy/gather/shaped/Slip handling, the
`^find_method` and wrap checks, argument decoding), so every warm call still paid
for them. For a call without arguments on an ordinary object the gate now runs
first (`try_plain_method_early_lane`); the probes that read per-instance state
(`message`, `value`, the renderers, the push family, ...) are refused by method
name, and a program with a user `^find_method` or any wrapped method keeps the
full path.

`$o.m()` on the #8880 repro, callgrind with the profiling build, warm second run,
difference of a 2,000 and a 1,000 iteration run: 14,874 -> 13,261 instructions per
iteration (-10.8%). `method_call` is still far from the issue's 10x rakudo
condition; the rest is the frame (`call_compiled_method_fast_in_unit`, ~6.3k) and
the dispatch tail (~2k).
