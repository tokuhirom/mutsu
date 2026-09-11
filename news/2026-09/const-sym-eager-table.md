# `const_sym` reads an eager table instead of probing two OnceLocks

`CompiledCode::const_sym` answers "what `Symbol` is the string constant at this
pool index" — the question every `CallFunc`, `CallMethod` and variable-access
opcode asks about its name. ADR-0037 Slice 1 made it a memo because the call
path had been re-interning routine names per call, and `Symbol::intern` is a
thread-local string-keyed hash: five of them per call had been ~26% of
`bench-fib`. The memo worked.

But the memo was reached lazily, through an outer `OnceLock` holding the side
table and a second `OnceLock` per slot holding the symbol. Once ADR-0037 and
ADR-0066 had cleared the interns and hash probes either side of it, the cost of
*reaching* the memo was what was left, and it was not small: 55.9M of
`bench-fib`'s 1,292.0M retired instructions, 4.33%.

That is the same lesson ADR-0066 records from the other direction — a
per-callsite inline cache reached through `OnceLock` → side table → index array
→ slot retired no fewer instructions than the two hash probes it replaced,
because five dependent loads cost what two hash probes cost. A shorter pointer
chase is not the fix; one load is.

## What changed

A chunk is finalized before it executes, so the table never needed to be lazy.
`compute_needs_env_sync` — which already walks the constant pool to decide
`reads_topic` — now interns every string constant in one pass and stores the
result in a plain `Box<[Option<Symbol>]>`. `const_sym` is an indexed load out of
that, marked `#[inline]` so each dispatch site gets the load itself rather than
a call; the intern fallback is `#[cold] #[inline(never)]` so the hash probe
stays behind a call.

`None` marks a non-string constant and an empty table marks a chunk that never
finalized — the hand-built `CompiledCode::new()` chunks in `runtime/`, whose
constant pools are empty or trivial. Both fall through to interning directly,
which is exactly what `const_sym` did before ADR-0037, so nothing regresses to
worse than the pre-memo behaviour.

## Measurement

callgrind, `--profile profiling`, at the merge of `69bedb4`. Retired
instructions are deterministic, so these are exact rather than sampled.

| | `bench-fib` total | vs base | `const_sym` | `bench-startup` |
| --- | ---: | ---: | ---: | ---: |
| base (`69bedb4`) | 1,291,994,518 | — | 55.9M (4.33%) | 8,737,119 |
| eager table | 1,264,035,861 | −2.16% | 28.0M (2.21%) | 8,744,395 |
| + `#[inline]` | **1,248,131,156** | **−3.40%** | **10.5M (0.84%)** | **8,737,168** |

The eager variant's risk was startup: it interns every string constant of every
chunk up front, including ones never called, and #7579 records `bench-startup`
as flat over the regression window. It is flat here too — 49 instructions above
base, 0.0006%. The intermediate row shows where that came from: the eager pass
itself costs ~7.3k instructions at startup, and inlining `const_sym` gives them
back.

The middle row is also why `#[inline]` is part of the fix rather than a
flourish. With the table eager but the function out of line, `bench-fib` paid a
call, a prologue and an epilogue 2.54M times — 11 retired instructions per call
for a read that is about six.

## Left on the table

`const_sym` is now ~0.84% of `bench-fib`, and roughly four fifths of that is
redundancy rather than cost per read: `exec_call_func_op_inner` calls
`const_sym(name_idx)` four times per dispatch (the `nqp::` flag check, the
unit-scoped-routine check, the light-cache key, and the argument to
`call_compiled_function_positional_light_at`) where one hoisted local would do.
That is worth about 0.7% and is a call-site cleanup, not a `const_sym` fix, so
it is left for whoever next picks up #7579.

Closes #7739.
