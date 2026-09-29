# A mention of `callsame` no longer slows down every multi method call

mutsu builds a multi method's deferral frame (the candidates that
`callsame`/`nextsame`/`callwith`/`nextwith`/`nextcallee`/`lastcall` walk) only
in a program that mentions one of those names somewhere. Once one did, even in
a comment, every multi method call built a full frame: the deferral
expansion, a signature match per candidate, a second resolution of the winner
and a fingerprint per candidate. Almost all of those frames were popped
without being read (#10108).

The frame cannot be skipped on the grounds that the winning candidate's body
never defers. The deferral builtins are dynamically scoped: a helper `sub`
called from the method body, or a closure the body calls, reaches the method's
next candidate exactly as the body would. Rakudo agrees.

A call now records only what the build needs (receiver class, method name,
invocant, arguments) and reserves its push-order token. The first deferral
builtin that runs builds every pending frame, in token order, into
`method_dispatch_stack`. Frames that turn out to have nothing to defer to are
settled as empty slots, so the builtins see exactly the stack an eager push
would have left (`src/runtime/method_dispatch_lazy.rs`).

A frame is built late only when that gives the same answer. Matching a
parameter that is `is rw`/`is raw`/sigilless, a typed `@`/`%` parameter, or a
native-typed one reads the call site (source variable names, their declared
types and the literal mask). A `where` clause, sub-signature or shape runs
user code in the current env. If any deferral candidate of a
`(class, method)` has such a parameter, its frames are still built at call
time. The answer is memoized per registry generation.

The #10108 repro, a three-candidate multi method called 30,000 times (release,
median of 7, run back to back):

| | without the comment | with `# callsame` |
| --- | ---: | ---: |
| before | 0.190 s | 0.716 s |
| after | 0.194 s | 0.198 s |

The multi method section of `benchmarks/bench-multi-dispatch.raku` went from
0.83 s to 0.20 s. It now takes the same time whether or not the operator
section that uses `callsame` is present.
