# A routine's inner `sub` is no longer displaced by another routine's same-named one

`Digest::SHA2`'s `sha256` computed a **wrong digest** — silently, no error — as
soon as a `sha384`/`sha512` call had run earlier in the same process and the
`sha256` body was re-entered. The RFC 4231 HMAC vectors caught it
(`Digest`'s `t/rfc4231.t`, one of the four bundled-library regressions in
[#7555](https://github.com/tokuhirom/mutsu/issues/7555)), where five of the
28 assertions came back with plausible-looking but incorrect bytes.

## What was actually wrong

Both compression functions in that module declare their helpers as inner subs of
the routine that uses them, and both use the same names:

```raku
multi sha256(blob8 $data, blob32 :$initial-hash = …) {
  sub rotr(uint32 $n, UInt $b) { $n +> $b +| $n +< (32 - $b) }
  sub Σ0 { rotr($^x,  2) +^ rotr($x, 13) +^ rotr($x, 22) }
  …
}
multi sha512(blob8 $data, blob64 :$initial-hash = …) {
  sub rotr($n, $b) { $n +> $b +| $n +< (64 - $b) }
  sub Σ0 { rotr($^x, 28) +^ rotr($x, 34) +^ rotr($x, 39) }
  …
}
```

In Raku each set is lexical to its own routine. In mutsu routines live in one
name-keyed registry (`Digest::SHA2::rotr`), and a routine scope snapshots that
registry on entry and restores the whole snapshot on exit — so after `sha512`
had run, the key could legitimately hold *its* `rotr` again while `sha256` was
about to run.

That is fine on its own: `sha256`'s body re-registers its own helpers each time
it runs. What broke was the registrar's **idempotent re-registration fast
path**, which exists so a `my sub` inside a hot routine is not re-derived on
every call. It asked two questions — "is the last fingerprint recorded under
this name mine?" and "is *something* registered under this name?" — and answered
"already installed" when both held. After a scope restore had put a *different*
declaration back under the name, both still held and the fast path returned
without installing anything. `sha256` then ran its whole compression loop
calling `sha512`'s 64-bit `rotr` and `Σ0`, producing a digest that is wrong in
every byte but has the right length.

The failure was invisible from inside the module: `$initial-hash`, the message
schedule `$w`, the round constants and the block count were all correct, which
is why the first investigation localized it to `subtest` and to `sha512`. The
one measurement that settles it is a `note` inside each `rotr` — under the
failing call it is `sha512`'s that prints.

## The fix

`registered_fn_fingerprints` now records the fingerprint **and the exact
`Arc<FunctionDef>` that was installed**, and the fast path takes the no-op only
when the registry still holds that same definition (`Arc::ptr_eq`). Presence
under the name is no longer accepted as proof of identity. A genuine
re-registration of an unchanged declaration still hits the fast path — the
pointer matches — so the hot `my sub` case keeps its shortcut; only the
displaced case now falls through to the full path, which is exactly what it
needs.

Pinned by `t/inner-sub-same-name-across-routines.t`, which runs the RFC 4231
case-4 and case-5 vectors in that order through the bundled `Digest`/`HMAC`
batteries.
