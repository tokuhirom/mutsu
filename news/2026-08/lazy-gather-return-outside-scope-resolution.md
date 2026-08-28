# Resolved: the swallowed lazy-gather `return` finding

`todo/deep/lazy-gather-return-outside-scope-swallowed-in-nested-block.md`
(filed while closing out `roast/S32-exceptions/misc2.t`'s
`MUTSU_REAL_TEST=1` residue) is resolved. Both minimal repros in the
original ticket now match `raku` exactly:

```raku
# Case 1 (bare block, message reached CLI uncaught before)
{
    EVAL 'my sub f() { gather { return } }; ~f()';
    say "unreached: eval did not throw";
    CATCH { default { say "caught: ", $_.^name } }
}
say "after";
# now: caught: X::ControlFlow::Return / after   (was: raw message escaping)

# Case 2 (closure through a plain user sub, signal silently swallowed before)
sub call-it(&code) { code(); }
call-it({
    EVAL 'my sub f() { gather { return } }; ~f()';
    say "unreached: eval did not throw";
    CATCH { default { say "caught: ", $_.^name } }
});
say "after";
# now: caught: X::ControlFlow::Return / after   (was: only "after", nothing caught)
```

The root cause split into two independent pieces, each with its own fix and
its own news entry:

- `try`/CATCH always let ANY `return` signal propagate past itself, with no
  way to tell a genuinely live one (still hunting for its target further up
  the stack — correct to let pass) from a dead one (target already exited —
  can never be caught by unwinding further, so must convert to a catchable
  `X::ControlFlow::Return` right there). See
  `gather-lazy-force-signal-delivery.md`.
- A `return` forced while reifying a lazy `gather` never had its target
  callable id resolved at all (unlike an ordinary closure's own `return`),
  so it fell back to "the first enclosing routine call frame catches it
  unconditionally" — silently absorbing it into an unrelated caller instead
  of ever raising an exception. Same entry.
- The ORIGINAL finding's "zen slice reifies where raku's does not" framing
  turned out to be a third, separable bug — an itemized scalar's sink
  forcing an as-yet-untouched lazy value that raku leaves alone. See
  `itemized-scalar-sink-does-not-force-lazy-gather.md`.

`roast/S32-exceptions/misc2.t`'s `X::ControlFlow::Return` subtest and
`roast/S02-types/array.t`'s `zen and whatever slices` subtest both now pass
under `MUTSU_REAL_TEST=1`; see `todo/deep/vendor-real-test-module.md`'s
2026-08-29 dated section for the measured before/after.
