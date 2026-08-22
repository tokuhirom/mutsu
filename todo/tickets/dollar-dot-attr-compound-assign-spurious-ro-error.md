# `$.attr *= 2` inside a method throws `X::Assignment::RO` where current raku allows it

Discovered via the doc-diff harness on `raku-doc/doc/Language/traps.rakudoc` (around line 212).
Note the doc's own `# OUTPUT:` comment is stale here (it says the trap *should* throw
"Cannot assign to an immutable value") — but re-verified directly, current `raku` does NOT
throw; it silently returns the original unmodified value. So the real, current-raku-verified
bug is that mutsu throws where raku doesn't (the doc's comment is drift, the underlying
behavior mismatch is real).

## Repro

```
class Point {
    has $.x;
    has $.y;
    method double {
        $.x *= 2;
        $.y *= 2;
        self;
    }
}
say Point.new(x => 1, y => -2).double.x
```

- raku: `1` (no exception — `$.x *= 2` inside the method silently doesn't mutate the read-only
  accessor-backed attribute, and execution continues past it)
- mutsu: throws `X::Assignment::RO: method 'x' is not rw`, so the whole program aborts with exit
  1

## Root cause guess

`$.x` inside a method is sugar for `self.x` (a method call through the public accessor), which
is read-only by default (no `is rw` on the attribute). raku evidently treats a compound-assign
(`*=`) through this read-only accessor as a silent no-op rather than a hard error, while mutsu's
compound-assign path raises `X::Assignment::RO`. This may be intentional in mutsu (arguably
"more correct"), but it diverges from raku's actual observed behavior.

## Affected files (starting point)

- `src/vm/vm_arith_ops.rs` / wherever compound-assignment operators (`*=`, `+=`, etc.) check
  target mutability for a `$.attr`-sugar method-call target

## Suggested next step

Confirm this exact behavior (no throw, silent no-op) is stable across a few raku point releases
before changing mutsu — if it's itself considered a raku bug/quirk that may get fixed upstream,
weigh whether matching it is worth the compatibility churn versus just documenting the
divergence.
