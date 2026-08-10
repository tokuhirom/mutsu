# A for-loop parameter inside an escaping closure reads the same-named captured OUTER lexical instead of the iteration value

## Minimal deterministic repro (11 lines, no dependencies)

```raku
sub make() {
    my $i = -1;
    my @parts = 1,;
    for 1..3 { $i++ }
    -> {
        for @parts -> $i {
            say "i=", $i;
        }
    }
}
make()();
```

raku prints `i=1` (the iteration value); mutsu prints `i=2` (the value the
OUTER `my $i` had when the closure was created). The closure captures the
enclosing `$i` as a free... it is NOT even a free variable of the closure —
the inner `for @parts -> $i` declares its own parameter — yet the captured
env entry for "i" wins over the per-iteration binding when the closure body
executes. Repro file: `tmp/loop-param-captured-shadow.raku`.

Sensitivities (verified): the outer `$i` must be MUTATED after
initialization (`$i++` in a loop — matching the shape where the outer `$i`
is a counter); the closure must escape `make()` and be invoked later.

## Real-world failure: `t/http-router-named-urls.t` (Cro::HTTP), 2 subtests

`Cro::HTTP::Router::LinkGenerator.rakumod`'s `signature-to-sub` builds
`@path-parts` (static segments) / `@fn-parts` (variable-segment indices)
using a counter `my $i = -1; for $s.params[] { ...; $i++; ... }`, then
returns the closure

```raku
-> *@args, *%nameds {
    my @result = @path-parts;
    for @fn-parts -> $i {
        @result[$i] = @args.shift;
        ...
    }
    ...
}
```

Under mutsu, the closure's `for @fn-parts -> $i` sees `$i` frozen at the
BUILD counter's final value: for route `-> 'search', $category, :$query`
(3 params, counter ends at 2) every iteration runs with `$i == 2`, so
`abs-link('qs', 'tools', ...)` produces `["search", Any, "tools"]` →
`/search//tools?...` instead of `/search/tools?...` ("Escaped named
param"); for `-> 'product', $id, 'docs', $file` (counter ends at 3) both
iterations write index 3 — the second overwrites the first, dropping `42`
→ `/product//docs/foo%20bar.jpg` ("Escaped positional"). Instrumented
shadow-lib trace confirmed: `S2S fn-parts=[1]` at build, `GEN loop i=2` at
call; `fn-parts=[1, 3]` at build, `i=3, i=3` at call — exactly the outer
counter's final values.

## Relationship to other open findings

Same family as `todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`
(closure-call captured-env merge vs. same-named bindings), but the opposite
direction: there a captured value LOSES to the caller env; here a captured
value WINS over the closure's own inner loop-parameter binding. A fix for
either should be checked against the other's repro. Also adjacent to
ADR-0023 (for-loop params as fresh per-iteration bindings) — the loop
param's read inside the closure body is apparently resolving by NAME
through the merged captured env (`cap_overrides` / `owned_captures` /
per-instance state installed at closure entry) instead of through the loop
binding.

## Verification (once fixed)

- The 11-line repro prints `i=1`.
- `t/http-router-named-urls.t` "Escaped named param" / "Escaped
  positional" pass (the file's rc=124 timeout at the end is a SEPARATE,
  still-undiagnosed issue — see the note in BLOCKERS/cro handoff; do not
  expect notok=0 rc=0 from this fix alone).
- roast: no regression in S04-statements/for*.t, S06-*/closure*.t.
