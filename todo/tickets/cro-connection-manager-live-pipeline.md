# Cro::Core composer.rakutest: 1 remaining failure (test 133) — thread-side attribute reads see a hollow instance

**Status 2026-07-31 (после the live-pipeline campaign):** composer.rakutest is
at **132/133, no hang** (was 83 + trailing hang at the start of the slice).
Landed in the same branch as this ticket update:

- chained-tap arm for ON-DEMAND whenever sources in the supply-block tap path
  (liveness propagates; the old cold replay snapshotted zero values and fired
  LAST immediately — the '(closed)' failure and the trailing hang);
- `supply_get_values` worklist: promise-source whenever markers block on the
  promise and run the body; nested markers re-queue (composite connector's
  `establish(...).list` returned the raw marker before);
- `lexical_closure_package` prefers the method's class when the innermost
  non-block routine frame is a method (nested-class short names in `start`
  bodies — `start Transform.new` in TestConnector.connect captured the
  CALLER's package);
- Bool smartmatch arm for type-object topics (`when $seen-connector` in
  Cro::CompositeConnector.BUILD classified everything as "before").

**Remaining failure (test 133, 'That message is a TestBinaryMessage'):**
`Cro.compose(TestUppercaseTransform, TestConnector, TestTransform)` →
`.establish(...).list` applies ONLY the connector's own transform. Root cause
narrowed with env-gated instrumentation: inside
`Cro::CompositeConnector.connect`'s `start` block, the attribute reads
`@!before` / `@!after` resolve against a `self` whose attributes are EMPTY
(`read_self_attr_cell` = None, env has no materialized key, and the instance
fallback finds nothing — debug print showed `self-keys=` empty), while the
same reads on the main thread (`.produces`) see the populated arrays. So the
start-thread's captured/cloned `self` is a hollow copy of the
CompositeConnector instance — possibly a pre-BUILD CoW snapshot or a
deep-copy in `clone_for_thread` that drops attribute contents (scalar
`$!connector` SURVIVES, arrays do not — suspicious of cell/array handling in
the thread clone or in the closure self materialization).

Repro (vendored Cro::Core lib):
`tmp/subset-repro/conn-probe.raku` variant with a before-transform — or
directly: `await $comp.connect(prepend => "x")` then `.components.elems`
(2 expected with one before-transform, observed 1).

Also unproven-but-kept in the same branch: closure captures now EXCLUDE
attribute-twigil env keys (`!x`, `@!x`, `%.x`, …) so attr reads go through the
live `self` instead of a creation-time snapshot; this did not resolve test
133 (the hollow-self read happens through the instance fallback too) but is
the honest semantics.
