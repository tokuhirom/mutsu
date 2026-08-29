# A `gather` body is compiled once, not on every creation

`exec_make_gather_op` compiles the `gather` block's body to bytecode so the
interpreter can force it natively. That compile ran **every time the `gather`
expression was evaluated** — so a `gather` inside a loop re-ran the whole
compiler on every iteration. It is the same shape as the `eval_map_over_items_rw`
bug fixed a few hours earlier (#7109), found by the same cheap oracle: on a
steady-state loop, `MUTSU_VM_STATS`'s `const-pool: add_constant=` must not grow
with the iteration count. It grew by **3 per gather creation** (60013 over a
20000-iteration loop).

## Measurement

20000 `gather` creations, each forced (release, `taskset -c 2`, best of 5):

| gather body | before | after | raku |
|---|---|---|---|
| 3 `take`s | 0.1003 | **0.0664** (−34%) | 0.2308 |
| + 20 extra statements | 0.7997 | **0.2344** (−71%) | 0.2747 |

The second row is the tell: mutsu's cost grew 8x with the body while rakudo's
grew 1.2x, because mutsu was paying a *compile* per creation and rakudo was not.
Afterwards mutsu is faster than rakudo on both. `add_constant` over the same run:
**60013 → 16**.

## The change

`Interpreter::gather_compile_cache` keys the compiled body on the pointer
identity of the body's analysis `CompiledCode` — the same key shape
`map_grep_compile_cache` uses, and for the same reason (the `Arc` is cloned into
the key so the address cannot be recycled under a live entry). It is a separate
map rather than a shared one because the compile *target* differs: a body that
declares routines is compiled through a wrapping `Stmt::Block`, whose
`BlockScope` restores the routine registry, so two sibling
`gather { sub foo {…} }` blocks do not collide. That decision is itself a pure
function of the body, so it stays inside the cached computation.

A `gather` whose body has no analysis chunk (an `EVAL`-built one) compiles fresh
— uncached but correct, exactly like the map/grep sibling.

Pin: `t/gather-body-compile-cache.t` — 15 assertions covering per-instance
capture, laziness (the body must not run eagerly), a body that declares a routine
(twice, with the same name, checking neither collides nor escapes), nested
gathers, `next`/`last` inside the body, empty and single-`take` gathers, and
re-reading a `.cache`d gather. Green under real `raku`.

## Found while here, not fixed

`state` inside a `gather` body is shared across gather instances in mutsu
(`loop=1,2,3` where rakudo gives `loop=1,1,1`). Verified to predate this cache
and be independent of it — mutsu compiled the body fresh each time and still
shared the cell, so the state key resolves by name rather than by chunk. Filed as
`todo/tickets/gather-block-state-is-shared-across-instances.md`.
