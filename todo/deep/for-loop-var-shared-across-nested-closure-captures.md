# A `for` loop's pointy-block variable is not frozen per-iteration when captured by a closure defined *inside another closure* built in the loop body

Discovered while re-measuring `t/http-router.rakutest` (vendored Cro::HTTP
suite) after fixing
`multipart-form-data-body-not-destructured-in-request-body-handler.md` — the
file now runs to completion (`1..439`) instead of dying partway through, and
this newly-reached test fails:

```
not ok 437 - The around blocks are called in top-to-bottom order
# Failed test 'The around blocks are called in top-to-bottom order'
# at t/http-router.rakutest line 2080
# expected: '12345'
#      got: '11355'
```

## Minimal Cro-independent repro (verified against real `raku`)

```raku
my $callback = -> { "base" };
for (10, 20) -> $v {
    $callback = -> $fn { -> { "$v:{$fn()}" } }($callback);
}
say $callback();   # raku: 20:10:base  — mutsu: 20:20:base
```

`raku`: `20:10:base`. `mutsu`: `20:20:base` — the first iteration's closure
(built when `$v == 10`) reports `$v == 20` once it is actually invoked (from
inside the second iteration's closure). The loop variable is not frozen at
closure-creation time for this nesting shape.

## What does and doesn't reproduce it

- A closure directly in the loop body, stored and called later, is fine:
  ```raku
  my @a;
  for (10, 20) -> $v { @a.push: -> { $v } }
  say @a.map({ $_() }).join(",");   # correct: 10,20 in both raku and mutsu
  ```
- A closure built by an IIFE *inside* the loop body, independently stored
  and called later, is also fine:
  ```raku
  my @b;
  for (10, 20) -> $v { @b.push: -> $fn { -> { "$v/$fn" } }("X") }
  say @b.map({ $_() }).join(",");   # correct: 10/X,20/X in both
  ```
- The same nested-closure shape *without* a loop (plain recursive-ish
  `sub` calls building the same chain) is also fine:
  ```raku
  sub make($v, $fn) { -> $ffn { -> { "$v:{$ffn()}" } }($fn) }
  my $c1 = make(10, -> { "base" });
  my $c2 = make(20, $c1);
  say $c2();   # correct: 20:10:base in both
  ```
- Only the combination of (a) a `for` loop pointy-block var, (b) captured by
  a closure that is itself created *inside another closure* built in the
  same loop body (the IIFE's returned block, not the IIFE's own body), and
  (c) that inner closure being invoked *later*, *nested inside* a
  later-iteration's closure (rather than called immediately or stored
  independently) — triggers the bug.

## Cro relevance

`Cro::HTTP::Router`'s `around` block chaining
(`lib/Cro/HTTP/Router.rakumod`, `RouteSet.transformer`, around line 220)
builds exactly this shape to chain multiple `around { ... }` blocks around a
handler:

```raku
for @!around.reverse -> $around {
    $callback = -> $fn { -> { $around($fn) } }($callback);
}
```

so any Cro app using two or more stacked `around` blocks gets the wrong
around-order (and, since the corrupted closure re-invokes the *last*
around's own body instead of chaining through each level, likely calls the
innermost handler zero or multiple times too — not yet verified in
isolation, only observed via the `$mark` string in the roast test).

## Root cause (confirmed by reading, not yet fixed)

This is a general closure/environment bug, not a Cro bug — no Cro code is
needed to reproduce it. Reproduce locally with `tmp/around-iife-repro4.raku`
(same 5 lines as above) — no Cro needed:

```
timeout 15 raku tmp/around-iife-repro4.raku          # 20:10:base
timeout 15 target/debug/mutsu tmp/around-iife-repro4.raku   # 20:20:base (wrong)
```

The per-iteration freeze mechanism for a `for`-loop pointy-block var is
`SubData::owned_captures` (`compute_owned_captures`,
`src/vm/vm_register_ops.rs:419`): a closure created *directly* inside a loop
body has its free vars that are also loop-local recorded in
`owned_captures`. At **call** time (`src/vm/vm_closure_dispatch.rs:415-419`)
these names get force-overwritten from the closure's own captured
`data.env`, winning over whatever the *calling* frame's env chain currently
holds under the same name (the ordinary merge at line 357-358,
`entry_or_insert_sym`, is a "don't overwrite if the caller already has this
name" merge — necessary for ordinary lexical shadowing, but wrong for a
loop-frozen value).

`compute_owned_captures` decides membership by intersecting the closure's
`free_var_syms` with `self.loop_local_vars` — but `self.loop_local_vars` is
**reset to empty across every call boundary** (`std::mem::take` at the top
of `vm_call_light.rs`, `vm_call_fast.rs`, `vm_call_light_typed.rs`,
`vm_run_loop.rs`, restored on return). That reset is correct for a callee's
*own* loop constructs, but it also means: when the IIFE (`-> $fn {...}`,
itself created directly in the loop body and correctly marked
`owned_captures = {$v}`) is invoked immediately, and *its* body then creates
the inner closure (`-> { "$v:{$fn()}" }`), that inner closure is created
**while `self.loop_local_vars` is empty** (we're inside the IIFE's own call
frame). So the inner closure's OWN `owned_captures` comes out **empty** —
even though `$v` genuinely has a per-iteration-frozen value at this point
(the IIFE's frame just force-installed it via ITS OWN `owned_captures`
override). The inner closure's `data.env` still captures the *correct*
value of `$v` at creation time (ordinary `capture_closure_env` is
unconditional, independent of `owned_captures`) — but because its
`owned_captures` list is empty, nothing forces that captured value to win
over the calling frame's env chain when the inner closure is *later*
invoked from a different context (nested inside the next iteration's own
closure chain, whose frame legitimately has `$v` bound to the *next*
iteration's value). The default `entry_or_insert_sym` merge then finds `$v`
already present via the calling frame's env-chain lookup and silently keeps
that (wrong, later-iteration) value instead of installing the inner
closure's own frozen one.

In short: `owned_captures`-ness is a property of "was this free var
loop-local at ANY ancestor closure-creation point in the chain", but the
current implementation only asks "is it loop-local in MY immediate creating
frame" — a nested closure loses the information the instant it is created
one level of closure-call deep from the loop body itself.

## Why this isn't a quick fix

The codebase already has an established pattern for exactly this shape of
problem — "cascade a per-frame vouched name-set down through nested closure
creation across call boundaries" — implemented for a *different* set,
`Interpreter::frame_authoritative` (`src/runtime/mod.rs:2009`,
`frame_authoritative_set` in `src/runtime/resolution_map_grep.rs:16`): a
frame's authoritative free vars plus any it inherited via
`SubData::authoritative_captures` are recorded on the interpreter and
consulted (`compute_authoritative_captures`) when a *further-nested* closure
is created inside that frame, letting the vouch cascade arbitrarily deep.

**The obvious-looking fix — reuse/mirror this pattern for `owned_captures`
— is explicitly documented as unsafe** by the comment on
`frame_authoritative_set` itself (`src/runtime/resolution_map_grep.rs:9-11`):

> unlike loop `owned_captures`, which may be concurrently-mutated shared
> cells and are deliberately NOT included (a reader thread would freeze a
> stale snapshot — `roast/S17-lowlevel/lock.t`'s condition-variable
> busy-wait).

So a correct fix needs a **separate** cascading mechanism for
`owned_captures` (not a naive reuse of `frame_authoritative`) that
reproduces the existing call-time override semantics (lines 415-419 of
`vm_closure_dispatch.rs`) while respecting the same constraint that made the
original authors exclude `owned_captures` from `frame_authoritative`: a
loop-owned capture can be a **shared mutable cell** (e.g. a `ContainerRef`
for a captured-and-mutated loop variable), and blindly cascading "this name
is loop-frozen, force-overwrite it" down through arbitrarily nested closures
could reintroduce the exact `S17-lowlevel/lock.t` stale-snapshot class of
bug the exclusion was written to prevent — the cascade must distinguish "the
per-iteration VALUE is frozen" (safe to cascade and force-overwrite) from
"the per-iteration BINDING is a live shared cell that a concurrent
reader/writer expects to still be live" (not safe to snapshot-and-force).

## Next step

Before touching `vm_closure_dispatch.rs` or `vm_register_ops.rs`, read
`docs/adr/` for whether a loop-capture/closure-environment ADR already
exists (none found as of this writing — `git grep -l owned_captures
docs/adr/` came back empty), and re-read
`roast/S17-lowlevel/lock.t` plus the commit that added the
`frame_authoritative` exclusion (`git log -S frame_authoritative
-- src/runtime/resolution_map_grep.rs`) to understand exactly what
concurrent-mutation shape it protects against, before designing the
parallel `owned_captures`-cascading mechanism this bug needs. This is
architecture-adjacent (a new interpreter-wide cascading capture-set,
mirroring but distinct from `frame_authoritative`) and touches
concurrency-sensitive code, so treat it as ADR-worthy rather than a
same-session patch.
