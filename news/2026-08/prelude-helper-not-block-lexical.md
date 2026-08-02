# A NativeCall prelude helper is not a block-lexical sub, and the batteries gate now runs on every PR

`DBIish/01-basic.rakutest` dropped from 35/35 to 27/35 in the bundled-library
gate: three assertions around `DBIish.install-driver('mysql')` began failing
with

```
Cannot load native library 'libmariadb.so.0'
```

where upstream expects a soft failure — `install-driver` succeeds and the
absence of the client library shows up only in `.version`, which is why the
suite's next assertion is `pass "Library not installed"`.

## Root cause

`inject_nativecall_subs_prelude` prepends `cglobal`, `nativecast`,
`nativesizeof`, `explicitly-manage` and `refresh` to any compunit whose source
mentions `NativeCall`. Each is stamped with the internal `__mutsu_prelude`
trait, which registers it under `GLOBAL` rather than the host compunit's
package: every module carries an identical copy and only the first registration
wins.

The block-lexical-sub escape hatch added in
`news/2026-08/regex-anchored-my-initializer-and-escaping-sub.md` took them
anyway. It stores a `Sub` value — with the declaring scope's env captured —
under a reserved key so a closure that outlives its block can still call the
routine. Applied to a prelude helper that meant one env-captured copy of
`cglobal` per NativeCall-using module, and the *last* one loaded answered every
later call. DBIish's mysql driver therefore probed its library through the
SQLite driver's scope.

The gate already had three conditions, each learned from a regression (not the
plain `&name`, not inside `EVAL`, not exported). This is the fourth, and it
shares the `is_export` one's reasoning: an interface routine is not lexical to a
block.

Release-only, because the failure is a wrong *closure env* rather than a wrong
answer — a debug build happened to reach a working copy. Pinned by
`t/prelude-helper-not-block-lexical.t`, which drives the shape in a subprocess
with the module directories on `-I`; the shape is delicate (reaching DBIish
through the bundled-battery path, or naming the loop variable instead of using
the topic, both hide it), so the pin was checked in both directions: it fails on
the unfixed binary and passes on the fixed one.

## The gate now runs on every PR

The regression reached `main` green. The bundled-library gate ran post-merge and
on PRs that touch the batteries — and the offending PR touched neither
`modules/` nor the gate's other paths, so its own CI never ran it. Post-merge
detection then put the breakage in front of whoever next opened a
batteries-touching PR, blocked by a regression they did not cause, on the branch
where it is hardest to attribute.

`ci.yml` therefore drops the path filter: the gate runs on every PR and every
push to `main`. The cost is what it always was — roughly 75 seconds of suites on
a job that has already built the release binary for roast — and a
documentation-only PR skips the whole job regardless.
