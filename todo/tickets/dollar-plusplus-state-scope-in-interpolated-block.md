# `$++` inside a string-interpolated `{...}` block doesn't reset per call the way raku's does

Discovered via the doc-diff harness on `raku-doc/doc/Language/traps.rakudoc` (around line 91).
This is one of the doc's actual "traps" — the surprising behavior IS the documented correct
one.

## Repro

```
sub count-it { say "Count is {$++}" }
count-it;
count-it;
```

- raku: `Count is 0` / `Count is 0` (the interpolated `{$++}` block is compiled once (it's the
  same source location every call), so its implicit `state`-like counter is tied to that single
  compiled closure/block object, not to the enclosing sub's call count — this is the documented
  "trap": naively you'd expect it to increment, but it doesn't)
- mutsu: `Count is 0` / `Count is 1` — mutsu's `$++` *does* increment across calls, meaning it
  doesn't reproduce raku's actual (surprising) per-block-instance scoping

## Root cause guess

`$++` is sugar for an implicitly-`state`-scoped auto-incrementing counter tied to its specific
source location. Inside a string-interpolated `{...}` block, raku apparently ties that state to
the block's *one-time compilation* in a way that makes it stable/shared appropriately, while
mutsu's implementation likely just uses a per-call-frame or globally-persistent state slot keyed
differently than raku's actual semantics.

## Affected files (starting point)

- `src/compiler/` / `src/runtime/` — `$++`/`$--` implicit state-variable implementation, and how
  it interacts with string-interpolated embedded blocks specifically (as opposed to `$++` used
  directly in a sub body, which may already behave differently — worth checking as a control
  case)

## Suggested next step

Check whether `sub count-it { state $c; say "Count is {$c++}" }` (an explicit outer `state`, not
`$++`) already gives the "expected" incrementing behavior in mutsu — if so, the bug is
specifically in how `$++`'s implicit state binds to an embedded interpolation block vs. a
directly-written sub body.
