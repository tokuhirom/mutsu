# The native cascades lose their last dead receiver steps

ADR-11276 slice 5 continues from the 2026-10-21 liveness probe. This round logged, for
every remaining block of the 0- and 1-argument cascades and of `try_native_method`, which
test files reach it (all 8,327 `t/` files and the 1,427 whitelisted roast files), then
reran the whole suite with groups of blocks skipped. Blocks whose absence changed no
answer are deleted:

- the 1-argument cascade's receiver step (no test reaches it) and both cascades' "the
  table answers, so decline" test, already done by the entries in front of them;
- the two-argument `answer_shapeless` branch of `try_native_method_raw`;
- the mixed-in role rows and the four `Cool` type-object/`Str`-text gates in
  `try_native_method`, which `call_method_with_values` carries as the one copy.

What the probe found live stays: the 0-argument receiver step (`Scalar`/itemized
containers), the `Seq` consume/cache checks, the `Int`-subclass
payload, the native integer coercers, `Code.Capture`/`clone`, the RakuAST accessors,
the `AT-POS` index-argument pair, the `Capture`/lazy-guard/ad-hoc/shadow/receiver guards
of `try_native_method_raw` and the `.REPR`/`.WHERE` guard for NativeCall handles (kept
for safety although no test needs it). Part of #11276.
