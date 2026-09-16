# A reentrant closure call could wipe out the caller's own captured array

Closing #8540. A combinator-style program that recurses through the *same*
compiled closure template — one activation still running its `.map` callback
while that callback calls back into another instance of the exact same
closure body — could silently lose a mutation made to a captured, boxed
`@`/`%` local:

```raku
sub make(&next) {
    -> @xs {
        my @flat;
        @xs.map(-> $x {
            my @got = &next(($x,));
            @flat.append($x, |@got);
        });
        @flat;
    }
}

my $leaf  = make(-> @xs { Empty });
my $outer = make($leaf);
say $outer((1,)).List;   # raku: (1 1)   mutsu: ()
```

The FunctionalParsers-derived reduction in the issue (`sequence` combinators
composed two levels deep) hit the same bug and returned `()` where raku
returns `((() (a:x (b:y c:z))))`.

## Root cause

`call_compiled_closure_in_unit`'s return path runs a cleanup loop over the
returning closure's own `cc.locals` and removes each one from the restored
caller env, so that a closure's own declaration (e.g. `my @flat;`) never
leaks into the caller under that bare name. The loop only ever excused a name
that the closure itself had *captured* under the same spelling
(`data.env.contains_key_sym(sym)`) — it never checked whether the CALLER's
own env, independent of anything this call did, already had a live binding
for that exact name.

That gap is invisible for an ordinary (non-reentrant) call, because the
env a closure's return restores is normally a fresh scoped overlay with no
such collision. But the native `.map`/`.grep` "rw" loop
(`eval_map_over_items_rw`) runs its block body inline via `run_reuse`,
directly against the *caller's own* (unscoped) env, to avoid full call-frame
overhead. When that inline body makes a real call into another instance of
the very same closure template — recursion through `&next` above — the
callee's own `my @flat;` is one of `cc.locals` for a CompiledCode that is
literally the caller's own compiled body too. Its exit cleanup then removed
`@flat` from the (shared, unscoped) env it was about to hand back — deleting
the caller's own, unrelated, live `@flat` binding along with it. The next
read of `@flat` inside the caller's still-running `.map` callback missed
entirely and silently auto-vivified a disconnected empty `Array` (mutsu's
undeclared-`@`-sigil-variable default), so the `.append()` a moment later
wrote to a throwaway array nobody could ever read back.

## Fix

The cleanup loop now also skips the removal when the name is already visible
through the (about to be restored) caller env: that can only be true because
the name is genuinely the caller's own, since the writeback merge earlier in
the same function already excludes exactly this closure's own locals from
ever being copied there in the first place. This targets precisely the
reentrant-recursion-through-a-shared-template case without touching the
(overwhelmingly more common) non-reentrant path, where the guard is simply
always false.

## Pin

`t/routines/closure/reentrant-closure-array-capture.t` — the minimized
`make`/`&next` shape above.
