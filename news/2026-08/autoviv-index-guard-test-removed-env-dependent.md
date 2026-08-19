# `t/autoviv-index-guard.t` removed: its verdict was owned by the kernel, not by mutsu

`t/autoviv-index-guard.t` (added in #3687, "guard array autoviv & string repeat
allocations") asserted that an absurd auto-vivifying index (`@a[9999999999999] = 1`)
and an absurd string repetition (`"x" x 99999999999999`) produce a catchable
`X::` instead of an uncatchable `handle_alloc_error` abort. It was deleted
because it did not test mutsu: whether it passed was decided by the host
kernel's memory policy.

`todo/tickets/autoviv-index-guard-hangs-locally.md` had recorded the symptom --
the file printed `1..13` and then hung until `timeout` killed it (exit 124),
deterministically, on the dev LXC, while CI stayed green -- with the root cause
left open. This is that root cause.

## What was actually happening

The guard is `Vec::try_reserve` (`Interpreter::autoviv_resize`), and
`try_reserve` reports failure only when the kernel *refuses the mapping*. Two
things decide that, and neither is under mutsu's control:

1. `vm.overcommit_memory`. Under the heuristic mode `0` (GitHub's runners) a
   single mapping larger than RAM + swap is refused, so the reservation fails
   and the catchable-error path fires. Under mode `1` -- always overcommit,
   which this dev container uses -- it is granted.
2. The ~128 TiB user address space. A request past it fails on every host no
   matter the overcommit mode.

`9999999999999` slots at 8 bytes is 80 TiB, which *fits* in the address space.
So on this container the reservation succeeded, and the `items.resize(new_len, fill)`
that follows started faulting in pages for real. Measured on the debug build,
sampling `/proc/<pid>/status` at 0.5 s intervals:

| case | `VmSize` | `VmRSS` after 3.5 s |
| --- | --- | --- |
| mutsu `"x" x 99999999999999` | 91 TiB | 12.6 GB |
| raku `@a[9999999999999] = 1` | 72 TiB | 17.5 GB |
| mutsu `@a[9999999999999] = 1` | 72 TiB | 4.1 GB (growing ~380 MB/s) |
| mutsu `@a[1, 9999999999999] = 1, 2` | 72 TiB | 2.4 GB |
| mutsu `@a[0][9999999999999] = 1` | 72 TiB | 2.4 GB |

RSS climbs until the machine dies; the file only ever "finished" because
`timeout` killed it. Under `ulimit -v 4194304` the same file completes in 0.06 s
with a 53 MB peak and all 13 tests pass -- the 4 GB address-space cap makes the
mapping fail, which is the only reason the guard engages. Note that raku is not
better here: it burns 17.5 GB on the same input before its own MoarVM panic.

The neighbouring `t/shaped-buf-alloc-guard.t` was never affected, and the
difference is only the magic number: it uses `99999999999999`, which is 800 TiB
for a shaped array and 100 TiB for a `Buf`, both past the address space, so its
allocations always fail fast. A test whose green/red is chosen by whether a
literal happens to land above or below the VA ceiling is not a test.

## Why deletion rather than a fix

- It pinned no spec behaviour. raku aborts with a MoarVM panic on the same
  input, and roast has no coverage of it -- `APPENDICES/A01-limits/misc.t` only
  requires that `"a" x 2**32-1` *lives*. Deleting costs zero Raku compatibility.
- Raising the literals until they clear 128 TiB would turn the file green
  everywhere while making it lie: `@a[1_000_000_000_000]` (8 TiB, comfortably
  inside the address space) would still take the machine down, and a green test
  would be claiming otherwise.
- Making it honest requires bounding the *request* up front, independently of
  the allocator. That is a real change with user-visible semantics (what is the
  maximum number of elements a mutsu array may be vivified to?), not a test fix,
  and it is not blocking anything today.

The `try_reserve` guards themselves were kept everywhere they are -- they cost
nothing and they do prevent the abort on hosts that refuse the mapping -- but
their doc comments now state plainly that they are best-effort mitigations
bounded by host policy, so the next reader does not write the same test again.
`t/shaped-buf-alloc-guard.t` gained a note explaining why its sizes must stay
above the address-space ceiling.

## Follow-up worth doing

raku caps string repetition deterministically, before allocating:

```
$ raku -e 'say ("a" x 4294967296).chars'
Repeat count (4294967296) cannot be greater than max allowed number of graphemes 4294967295
$ raku -e 'say ("ab" x 3000000000).chars'
Can't repeat string, required number of graphemes (2 * 3000000000) greater than max allowed of 4294967295
```

Both checks are allocator-independent and cost nothing at runtime. Adopting them
would make mutsu's string-repeat behaviour match raku exactly and would remove
the 91 TiB reservation from that path for good; it is marked with a `TODO` at
the guard site in `src/vm/vm_arith_int_ops.rs`. The array side has no raku
precedent to copy -- raku simply panics -- so a cap there needs a decision of
its own.
