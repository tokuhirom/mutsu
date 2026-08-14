# P5tie needs a real container-tie protocol; `array.rakutest` also hits a parse bug

## Symptom

`P5tie`'s test suite (un-triaged `test_die` row in
[todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
splits into two independent problems, both confirmed against a clean raku
baseline (`raku -I lib t/<file>` passes all subtests for all three files):

### 1. `scalar.rakutest` / `hash.rakutest`: `Stash` has no `BIND-KEY`

```
No such method 'BIND-KEY' for invocant of type 'Stash'
  in sub tie at lib/P5tie.rakumod line 38   # scalar.rakutest
  in sub tie at lib/P5tie.rakumod line 311  # hash.rakutest
```

`P5tie` implements Perl 5's `tie()` by binding a variable's storage to a
user-supplied class instance (`TIESCALAR`/`TIEARRAY`/`TIEHASH` + `FETCH`/
`STORE`/... trap methods) via Raku's low-level container-binding protocol —
`BIND-KEY` on a `Stash` (a package/lexical-pad reflection object) is part of
that machinery in real Raku's CORE.setting. mutsu's `Stash`-equivalent value
does not implement `BIND-KEY` at all.

### 2. `array.rakutest`: parse-time failure before any test runs

```
Runtime error: X::Syntax::NoSelf
```

This is a **separate, unrelated** bug — a parse/compile-time error, not a
missing method. Needs its own minimal repro/bisection (not done yet); do not
assume it shares a root cause with the `BIND-KEY` gap above.

## Why this needs a design pass

Implementing `tie()` properly means implementing enough of the real
container-binding protocol (`BIND-KEY`/`BIND-POS` or whatever the actual
underlying primitive is on a `Scalar`/`Array`/`Hash` container, dispatching
through to a user-supplied proxy object's FETCH/STORE) generally — not just
enough to make this one dist's trap methods fire. This is genuine MOP/
container-model work, not a quick patch. Per `CLAUDE.md`'s BATTERIES.md
rung-3 ban, `tie` semantics should be real interpreter machinery (rung 2),
not a native P5tie-specific stopgap.

## Next steps (not started)

1. Bisect `array.rakutest`'s `X::Syntax::NoSelf` down to a minimal repro
   independent of `tie` — it fails before any `tie` call, so it is likely a
   plain parser bug in whatever array.rakutest's early lines do differently
   from scalar.rakutest/hash.rakutest (diff the two files' preambles).
2. For the `BIND-KEY` gap: find/design whatever the real container-binding
   primitive should be (a `ContainerRef`-level operation, given mutsu already
   has a `ContainerRef` cell abstraction for aliasing — see ADR-0013 §7) and
   implement `Stash.BIND-KEY` (and any sibling ops P5tie's `tie`/`untie`/
   `tied` need) in terms of it.

## Repro

```
cd <extracted P5tie dist>
raku -I lib t/scalar.rakutest   # passes, 21 subtests via TAP
target/debug/mutsu -I lib t/scalar.rakutest   # dies: No such method 'BIND-KEY'
target/debug/mutsu -I lib t/array.rakutest    # dies at parse time: X::Syntax::NoSelf
```

Dist tarball cached at
`~/.cache/mutsu-dist-sweep/P_5T_P5TIE_*.tar.gz` (from
`scripts/dist-compat-sweep.py`'s cache; re-run the sweep or extract that
tarball to reproduce — not vendored into this repo).
