# `samewith` inside a lazy `gather` redispatches the right routine

`samewith` is *lexical* in Rakudo: it redispatches `&?ROUTINE`, the routine the
call was written in. mutsu implemented it purely dynamically — a
`samewith_context_stack` pushed on entering a multi candidate and popped on the
way out — which is indistinguishable from the lexical rule right up until a
closure created inside the routine runs after the routine has returned. A lazy
`gather` body is exactly that closure, and it failed in two different ways
depending on who forced it.

**Forced from an outer scope, it died.** The frame it needed had been popped:

    proto K($x, $len?) {*}
    multi K($x)       { gather { take $x; take $x + 1 } }
    multi K($x, $len) { gather for samewith($x) { take $_ * $len } }
    say K(3, 10).list;   # samewith called outside of a dispatch context

**Forced while some other routine was still on the stack, it silently
redispatched THAT routine** — the worse failure, because nothing looked wrong
until the arguments did not fit. `Digest::SHA3`'s `Keccak` is called from
`sha3_256`, and its output stage is
`gather for samewith $inputBytes, :$delimitedSuffix, :$rate, :$capacity {...}`;
the `.list` that forces it runs inside `sha3_256`, so the top of the stack was
`sha3_256` and mutsu called *that* with `Keccak`'s named arguments, reporting
`Unexpected named argument 'delimitedSuffix' passed`.

## The fix

`exec_make_gather_op` records the innermost samewith context into the env
snapshot it already takes for the gather body
(`__mutsu_samewith_lexical_name`, plus `__mutsu_samewith_lexical_invocant` for a
method), and every path that runs a gather body — `force_lazy_list_vm`,
`force_lazy_list_vm_n`, `force_lazy_list` and `force_lazy_list_prefix_bridge` —
re-pushes that context for the duration of the run.

Re-pushing, rather than consulting the env at the `samewith` call site, is what
keeps the rest of the semantics intact: a routine *called from* the body pushes
its own frame on top, so its own `samewith` still means itself, and the frame
disappears again when the force returns. The capture also lives in the gather's
own env, so two gathers created in two different routines each redispatch their
own routine.

## What it unblocks

This is the remaining half of `todo/tickets/digest-dist-blockers.md` §6.
Together with the named-dispatch fix
(`news/2026-08/multi-named-narrowness-declaration-order.md`), `Digest::SHA3`'s
`sha3_256` now runs all the way through `Keccak` into `KeccakF1600`, where it
stops on an unrelated `Cannot modify an immutable value` — recorded on the
ticket.

Pinned by `t/samewith-inside-lazy-gather.t` (8 tests, all of which also pass
under `raku`), whose last case uses `t/lib/SamewithGatherModule.rakumod` to
reproduce the `Digest::SHA3` shape exactly: a module-private `proto` redispatched
from a gather that the exported entry point forces.
