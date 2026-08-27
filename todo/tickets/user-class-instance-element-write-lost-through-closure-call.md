# An element write into a user-class instance is lost when it happens inside a closure another routine invokes

`my $m = MyHash.new; lives-ok { $m<a> = 1 }, '...'` leaves `$m` empty. The same
write into a builtin `Hash.new` instance, into a `%h` hash, into an `@a` array
or into a `$scalar` all write back correctly — only a **user-declared class
instance held in a `$` scalar** loses it.

## Repro (needs `MUTSU_REAL_TEST=1`, because it needs a closure-call boundary)

```raku
use Test;
plan 1;
class Map is Hash { }
my %h;             lives-ok { %h<a>  = 1 }, 'plain hash';
my $sc = 0;        lives-ok { $sc    = 2 }, 'scalar';
my @ar;            lives-ok { @ar.push(3) }, 'array push';
my $hh = Hash.new; lives-ok { $hh<a> = 4 }, 'Hash.new instance';
my $um = Map.new;  lives-ok { $um<a> = 5 }, 'user Map instance';
my $um2 = Map.new; $um2<a> = 6;                 # no closure
say %h.raku; say $sc.raku; say @ar.raku; say $hh.raku; say $um.raku; say $um2.raku;
```

```
                 mutsu (MUTSU_REAL_TEST=1)   raku
plain hash       {:a(1)}                     {:a(1)}
scalar           2                           2
array            [3]                         [3]
Hash.new         ${:a(4)}                    ${:a(4)}
user Map         {}            <-- LOST      ${:a(5)}
user Map direct  {:a(6)}                     ${:a(6)}
```

Under mutsu's native `Test` provider every line is correct, because the native
`lives-ok` does not invoke the block through a Raku routine boundary. The
genuine upstream `Test.rakumod`'s `lives-ok` is
`multi sub lives-ok(Callable $code, $reason = '') { ... try { $code(); } ... }`,
so it does — that is the whole of its involvement. Nothing here is
Test-specific.

## What has already been ruled out (do not re-derive these)

Hand-written twins of `lives-ok` do **not** reproduce it, which is why the
minimal repro above still goes through the real module. Each of the following
was built and measured, and all of them write back correctly:

- a local `sub f(Callable $code) { $code() }` and a local
  `sub f(Callable $code) { try { $code(); } }`;
- the same two declared in a separate module and imported (so it is not the
  imported-routine / statement-call dispatch path that bit the `:name<90>`
  allomorph bug in `news`/`todo` for the 2026-08-28 slice);
- the listop call form (`f { ... }, 'x'`) as well as the parenthesised one;
- a `multi` with a `Callable $code, $reason = ''` signature;
- adding module-scoped variable writes (`$time_after = 1`) and a second sub
  call (`proclaim`-shaped) around the `try`, i.e. the rest of real `lives-ok`'s
  body.

So the trigger is something the real `Test.rakumod` does that none of those
twins reproduce — a plausible next suspect is the `proto`/multi-candidate
dispatch that the real module's much larger candidate set goes through, which
would pass the block through a `|capture` and could copy the instance.

## Why it matters

`$obj<key> = value` inside a callback is ordinary code. The bug is invisible
today only because mutsu's own `t/` suite mostly uses the native provider, and
because the affected shape needs both a user class *and* a closure-call
boundary. `t/user-class-shadows-immutable-builtin.t` fails 5 of its 14 subtests
under the real module purely because of this (`todo/deep/vendor-real-test-module.md`).

## Where to look

The write goes through the element-assign path for an `Instance` receiver held
in a scalar slot; the closure's captured `$um` must be a shared container cell
rather than a by-value copy. `src/compiler/expr_call.rs`'s
`is_closure_literal_arg` escape marking and the `capture_var_cell_inner`
machinery are the relevant surface. Note that the escape marking is
deliberately narrower on the statement-call path (see the comment in
`compile_tail_stmt_call_value`, and `t/bind-alias-chain.t`, which regressed the
last time it was widened) — so the fix probably is not "mark more things
escaping" but "an Instance receiver's element store must reach the shared
container".
