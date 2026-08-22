# `is cached` trait does not memoize — every call re-executes the body

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Routine.rakudoc:144`).

## Repro

```raku
use experimental :cached;

sub nth-prime(Int:D $x where * > 0) is cached {
    say "Calculating {$x}th prime";
    return (2..*).grep(*.is-prime)[$x - 1];
}

say nth-prime(43);
say nth-prime(43);
say nth-prime(43);
```

- raku: prints `Calculating 43th prime` once, then `191` three times (the second and third
  calls hit the cache and skip the body, including the `say`).
- mutsu: prints `Calculating 43th prime` / `191` three times — every call re-executes the full
  body, i.e. the `is cached` trait has no effect.

## Root cause hypothesis

`is cached` (gated behind `use experimental :cached`) should wrap the routine so subsequent
calls with identical arguments look up a per-signature cache keyed by the argument values and
return the memoized result without re-invoking the body. mutsu likely does not implement this
trait at all — it is probably accepted as a no-op unknown trait (parses fine, has zero runtime
effect) rather than wiring an actual memoization wrapper around the compiled sub.

## Affected files (starting point)

- Trait registration/dispatch for routine traits (`is rw`, `is pure`, etc.) — grep for how
  `is rw`/`is pure` are recognized on a `sub`/`method` declaration to find where a new `is
  cached` case would plug in.
- Needs a real memoization cache (keyed by argument values, presumably per-declaration-site),
  not a stub — this is a genuine caching feature, not cosmetic.
