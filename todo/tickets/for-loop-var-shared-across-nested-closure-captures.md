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

## Next step

This is a general closure/environment bug, not a Cro bug — no Cro code
needed to reproduce it (see the 5-line repro above). Root-cause it with
`rust-gdb` breakpoints on the closure-creation site (`compiler/expr_closure.rs`
/ `vm/vm_closure_dispatch.rs`) comparing what free-variable value gets
captured for `$v`/`$around` at each iteration vs. what is read back at call
time — likely another instance of the "for-loop pointy-block param captured
by reference into a shared env slot instead of being frozen per iteration"
family documented for simpler shapes elsewhere (see `loop-var-closure-capture.t`,
which already passes — so whatever mechanism protects the simple case does
not extend to a closure created inside another closure inside the loop
body). Reproduce locally with `tmp/around-iife-repro4.raku` (also copied
below) — no Cro needed:

```
timeout 15 raku tmp/around-iife-repro4.raku          # 20:10:base
timeout 15 target/debug/mutsu tmp/around-iife-repro4.raku   # 20:20:base (wrong)
```
