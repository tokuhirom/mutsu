# A handled Failure's `.fail` METHOD throws — DBIish 38-pg-errors 8 → 9/9 (Pg suite 10/11)

2026-07-29. The last failing subtest of DBIish's `t/38-pg-errors.rakutest`
("Raise Temporary Exception") came down to a Failure-semantics divergence,
found by shrinking DBDish's execute error path to a DB-free repro:

```raku
method run() {
    with self!seterr() { "never" } else { .fail }
}
throws-like { my $r = $k.run; CATCH { default { .rethrow } } }, X::My;
```

The `with` marks the Failure handled (via `.defined`), and in raku the
**method form `.fail` on a handled Failure throws its wrapped exception
outright** — while the **sub form `fail $f` re-arms it as a passive Failure**
even when handled (pinned by roast S04-exceptions/fail.t's
`foo() orelse fail $_` / `.&fail`, which both route through the sub). mutsu
treated both forms as re-arm, so `execute` returned a passive Failure that a
CATCH-carrying `throws-like` block never saw — "code dies" failed. The
method-dispatch `.fail` arm now throws for a handled Failure invocant;
`builtin_fail` (the sub form) still re-arms.

Wrong turns worth recording:

- A DBG probe showed `RaiseError` is falsy on the statement handle in raku
  too — both implementations take the `.fail` branch of
  `$!RaiseError and .throw or .fail`; only what `.fail` then does differs.
- An attempted "fix" that made CATCH-carrying blocks yield their tail
  VarDecl's value (so ThrowIfFailure would see it) was REVERTED after
  checking raku: `do { my $x = 42; CATCH {} }` is Nil there, and a tail
  `my $r = flaky()` Failure does NOT reach the block's own CATCH.
- The first cut put the handled-throw into `builtin_fail` (both forms) and
  roast's re-arm tests caught it locally — the method/sub distinction IS the
  spec. Measure each form against raku before generalizing.

Pin: `t/failure-handled-refail-throws.t` (passes under raku too). DBIish's
upstream Pg suite is now **10/11** files at raku parity; only `36-pg-enum`'s
closure-capture staleness remains.
