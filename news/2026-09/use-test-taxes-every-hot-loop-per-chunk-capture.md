# The reflective closure capture becomes a per-chunk decision

Loading a module made unrelated hot loops in the importing file slower in
proportion to how much that module exported ([#7565]). The measurement that
named the problem was a 20 000-iteration loop in a file whose only other
content was `use Test`, and three mechanisms fed it. #7656 took the first (the
END-phaser refresh's string round-trip). This takes the remaining two, which
were two faces of one thing: *the capture is as wide as the creating scope*.

## A process-global latch decided a per-closure question

`opcode::reflective_name_access_possible()` is a monotonic, process-global flag,
set at compile time when any chunk anywhere contains a spelling that can reach a
lexical by a dynamic name -- `EVAL`, `CALLER::`/`OUTER::`, a symbolic deref, a
pseudo-stash, an indirect code lookup. `capture_closure_env` read it to decide
whether a closure must snapshot the WHOLE visible env by name instead of the
filtered upvalue set.

That is the wrong granularity. Upstream `Test.rakumod`'s `throws-like` contains
an `EVAL`, so a bare `use Test` latched the flag for the rest of the process and
every closure in the program -- including `sub ($v) { $v + 1000 }`, which has no
free variables at all -- captured the importer's entire env, then re-merged it
key by key into the callee's overlay on every call.

The scan that sets the global flag now also records its answer on the chunk it
scanned (`CompiledCode::needs_reflective_capture`), and the capture requires
both. The per-chunk answer is deliberately wider than the global one, because a
missed case here is a silently truncated lexical view rather than the merely
conservative over-set the global flag can afford: it takes the global flag's op
scan, plus `uses_callframe`, plus the `gather`/`whenever`/`package` bodies that
live in `stmt_pool` where no free-variable scan can see them, plus a
constant-pool scan for the reflective names (the same sound over-approximation
`reads_topic` already uses, which catches `$code.EVAL` and the other method-call
spellings), plus the flag of every closure literal nested in the chunk.

## Two thirds of what the capture kept was routine-registration metadata

With the narrow path restored, the capture of a closure created inside a routine
in that file still held 90 entries -- and 55 of them were
`__mutsu_callable_id::Test::<name>`, one per routine `use Test` had made visible.
No closure body can name one: they are registration markers, and every consumer
(`sub_state_scope_id`, `registration_clone_id`, the call-eligibility probe) reads
them from the *live* env at call time, where the frame chain still has them. They
are excluded from the capture now, marked by a new `Symbol::flags` bit so the
filter costs no string scan. The one case that genuinely needs the marker carried
-- a closure escaping a `use`-inside-`EVAL` scope that `PopImportScope` is about
to strip -- is pinned by name in `capture_bare_callees`, alongside the `&name`
alias it already pinned. That gate also learned to pin an alias whose registry
entry is *already* gone, which is the state a closure created by an outer
EVAL-returned closure runs in (`t/eval-closure-imported-sub.t`).

## The END-phaser refresh still flattened the env it only ever indexed

`call_compiled_closure_in_unit` ends by refreshing END phasers' captured envs for
the returning closure's captured names. It read the live values through
`clone_env()` -- a whole-scope flatten, plus its drop, on every closure return in
any program that has an `END` (which `use Test` also supplies, as its plan
check). But `update_end_phaser_envs_for_keys` looks the values up one key at a
time with `get_sym`, and a scoped env's `get_sym` already walks its parent chain:
it needs the full lexical *view*, not a flat *map*. Loaning the live env out and
back is O(1).

## Measured

Callgrind, release, `MUTSU_JIT=off`, `MUTSU_GC=off`, warm precompilation, slope
between 6 000 and 14 000 iterations of the ticket's loop:

| | instructions per iteration |
| --- | --- |
| the loop with no `use Test` (the floor) | 74 908 |
| `+ use Test`, before | 121 419 |
| `+ use Test`, after | **105 391** |

The tax the ticket is about -- what `use Test` adds to a loop that never calls
into it -- falls from 46 511 to 30 694 instructions per iteration, a 34%
reduction, and the loop's total cost falls 13%. The no-`use Test` floor is
unchanged (74 908 -> 74 697).

What remains is the constant part of the capture: about twenty built-in dynamics
(`$*IN`, `$*OUT`, `%*ENV`, ...) and the `__mutsu_type::` constraint keys, which
still scale with the importing scope. Making those cheap needs a capture that
does not walk the whole env at all; a per-tier memo of the key-only filter was
built and measured for this change and did NOT pay -- the tier it would memoize
is written on every loop iteration, so the memo never armed. That is recorded on
the issue, which stays open.

[#7565]: https://github.com/tokuhirom/mutsu/issues/7565
