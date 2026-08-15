# `Digest`-adjacent `RSV` dist: `from-rsv` results compare `$[[Any],]` vs raku's `[[Any],]`

Found while fixing
`news/2026-08/sigilless-constant-invisible-in-nested-sub-inside-module.md`
(unrelated — that fix resolved 14/16 of this dist's `t/simple-cases.rakutest`
failures; these remaining 5 are a separate bug).

## Symptom

`~/.cache/mutsu-dist-sweep/R_SV_RSV_*.tar.gz` (`lib/RSV.rakumod`'s `from-rsv`),
run via:

```sh
D=~/.cache/mutsu-dist-sweep   # extract the RSV tarball first
timeout 30 target/debug/mutsu -I $D/<extracted>/lib $D/<extracted>/t/simple-cases.rakutest
```

5 of 16 subtests fail with a container-identity mismatch, e.g.:

```
not ok 8 - from-rsv: [[Any],]
# expected: [[Any],]
#      got: $[[Any],]
```

The underlying byte-level decode is CORRECT (confirmed via the diagnostic
`say`: `RSV: Blob[uint8]:0x<FE FF FD>` matches raku) — this is purely an
`is-deeply` container-shape mismatch between mutsu's `from-rsv` return value
(itemized as `$[...]`, i.e. wrapped in a Scalar container) and raku's (a bare
`List`, no `$`-itemization).

## Not yet minimally reduced

A simplified standalone repro (`sub f { my @rows; @rows.push: [Any]; @rows }`)
does NOT reproduce the extra `$` — both raku and mutsu print `$[[Any],]` for
that shape, so the actual trigger is something more specific in `from-rsv`'s
real control flow (likely involving `gather`/`take`, a `Slip`, a `.List`
coercion, or a `Rakudo::Internals`-style helper the dist uses — `lib/RSV.rakumod`
needs to be read to find the actual shape).

## Where to look

`lib/RSV.rakumod`'s `from-rsv` — trace what it returns (`gather`/`take`?
explicit `.list`? a `for` loop building `@rows`?) and diff mutsu vs raku's
itemization at each intermediate step, matching the general pattern other
container-identity (§3) tickets in this codebase have used.
