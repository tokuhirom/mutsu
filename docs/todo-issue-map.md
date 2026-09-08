# `todo/` to issue map (frozen)

The in-repo `todo/` directory was retired on 2026-09-08 in favour of GitHub
issues; see [issue-workflow.md](issue-workflow.md) for how the queue is run now.

Older ADRs, `news/` entries and source comments cite the findings by their
file path. This table resolves those paths, and is **frozen** — it records
the state at migration, so do not add rows for issues filed afterwards and do
not remove a row when its issue closes. A closed issue still resolves; that
is the whole reason for the move.

A `todo/...md` path that is *not* in this table belonged to a finding that
was already resolved before the migration: look for `news/YYYY-MM/<slug>.md`,
which keeps the same slug.

| file | issue |
| --- | --- |
| `todo/deep/adr0019-e2-e4-resolver-core.md` | [#7540](https://github.com/tokuhirom/mutsu/issues/7540) |
| `todo/deep/adr0039-slice2-container-reads-compile-to-a-slot.md` | [#7541](https://github.com/tokuhirom/mutsu/issues/7541) |
| `todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md` | [#7546](https://github.com/tokuhirom/mutsu/issues/7546) |
| `todo/deep/config-toml-battery-core-blockers.md` | [#7539](https://github.com/tokuhirom/mutsu/issues/7539) |
| `todo/deep/containerref-holding-a-hash-is-indistinguishable-from-itemization.md` | [#7542](https://github.com/tokuhirom/mutsu/issues/7542) |
| `todo/deep/exception-class-hierarchy-is-mostly-unregistered.md` | [#7545](https://github.com/tokuhirom/mutsu/issues/7545) |
| `todo/deep/gc-contents-mut-cross-thread-aliased-writes.md` | [#7543](https://github.com/tokuhirom/mutsu/issues/7543) |
| `todo/deep/hash-copy-allocates-a-string-per-key.md` | [#7549](https://github.com/tokuhirom/mutsu/issues/7549) |
| `todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` | [#7556](https://github.com/tokuhirom/mutsu/issues/7556) |
| `todo/deep/lsp-references-needs-a-side-table-not-ast-spans.md` | [#7547](https://github.com/tokuhirom/mutsu/issues/7547) |
| `todo/deep/metamodel-roles-are-not-composable-types.md` | [#7551](https://github.com/tokuhirom/mutsu/issues/7551) |
| `todo/deep/module-toplevel-private-sub-leak-cleanup.md` | [#7558](https://github.com/tokuhirom/mutsu/issues/7558) |
| `todo/deep/native-method-accepted-named-declarations.md` | [#7544](https://github.com/tokuhirom/mutsu/issues/7544) |
| `todo/deep/nativecall-cannot-be-vendored.md` | [#7560](https://github.com/tokuhirom/mutsu/issues/7560) |
| `todo/deep/ordered-alternation-eager-candidate-enumeration.md` | [#7548](https://github.com/tokuhirom/mutsu/issues/7548) |
| `todo/deep/rakuast-remaining.md` | [#7564](https://github.com/tokuhirom/mutsu/issues/7564) |
| `todo/deep/retire-the-native-test-provider.md` | [#7566](https://github.com/tokuhirom/mutsu/issues/7566) |
| `todo/deep/role-body-placeholder-mu-supply.md` | [#7550](https://github.com/tokuhirom/mutsu/issues/7550) |
| `todo/deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md` | [#7567](https://github.com/tokuhirom/mutsu/issues/7567) |
| `todo/deep/subtest-compiled-dispatch-async-middleware-regression.md` | [#7552](https://github.com/tokuhirom/mutsu/issues/7552) |
| `todo/deep/template-engines-blocked-on-mutsu.md` | [#7553](https://github.com/tokuhirom/mutsu/issues/7553) |
| `todo/deep/unify-block-statement-and-value-compilation.md` | [#7569](https://github.com/tokuhirom/mutsu/issues/7569) |
| `todo/deep/vendor-real-test-module-flip.md` | [#7554](https://github.com/tokuhirom/mutsu/issues/7554) |
| `todo/deep/vendored-test-battery-gate-regressions.md` | [#7555](https://github.com/tokuhirom/mutsu/issues/7555) |
| `todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md` | [#7561](https://github.com/tokuhirom/mutsu/issues/7561) |
| `todo/perf/bench-ctor-construction-parity.md` | [#7568](https://github.com/tokuhirom/mutsu/issues/7568) |
| `todo/perf/closure-literal-creation-cost.md` | [#7557](https://github.com/tokuhirom/mutsu/issues/7557) |
| `todo/perf/digest-ripemd-start-per-block-overhead.md` | [#7571](https://github.com/tokuhirom/mutsu/issues/7571) |
| `todo/perf/hash-access-diffuse-regression-2026-09.md` | [#7559](https://github.com/tokuhirom/mutsu/issues/7559) |
| `todo/perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md` | [#7570](https://github.com/tokuhirom/mutsu/issues/7570) |
| `todo/perf/interpreter-call-path-in-hot-loops.md` | [#7573](https://github.com/tokuhirom/mutsu/issues/7573) |
| `todo/perf/interpreter-new-is-expensive-and-retains-memory.md` | [#7572](https://github.com/tokuhirom/mutsu/issues/7572) |
| `todo/perf/late-august-call-path-slowdown-remainder.md` | [#7579](https://github.com/tokuhirom/mutsu/issues/7579) |
| `todo/perf/listop-call-bypasses-every-compiled-call-cache.md` | [#7574](https://github.com/tokuhirom/mutsu/issues/7574) |
| `todo/perf/locals-frame-is-a-pooled-vec-not-a-register-window.md` | [#7562](https://github.com/tokuhirom/mutsu/issues/7562) |
| `todo/perf/method-dispatch-flattens-the-env-on-every-call.md` | [#7563](https://github.com/tokuhirom/mutsu/issues/7563) |
| `todo/perf/non-constant-defaults-still-forfeit-the-light-path.md` | [#7581](https://github.com/tokuhirom/mutsu/issues/7581) |
| `todo/perf/regex-inline-code-carrier-prologue-overhead.md` | [#7575](https://github.com/tokuhirom/mutsu/issues/7575) |
| `todo/perf/use-test-taxes-every-hot-loop-in-the-file.md` | [#7565](https://github.com/tokuhirom/mutsu/issues/7565) |
| `todo/perf/yaml-parse-throughput.md` | [#7576](https://github.com/tokuhirom/mutsu/issues/7576) |
| `todo/tickets/array-slice-with-a-runtime-empty-reversed-range-hangs.md` | [#7578](https://github.com/tokuhirom/mutsu/issues/7578) |
| `todo/tickets/big-denominator-rat-str-truncates-to-f64.md` | [#7577](https://github.com/tokuhirom/mutsu/issues/7577) |
| `todo/tickets/block-scoped-use-drops-a-nested-modules-imports.md` | [#7580](https://github.com/tokuhirom/mutsu/issues/7580) |
| `todo/tickets/channel-supply-tap-done-callback-never-fires.md` | [#7584](https://github.com/tokuhirom/mutsu/issues/7584) |
| `todo/tickets/code-object-renders-as-nothing-inside-a-list.md` | [#7587](https://github.com/tokuhirom/mutsu/issues/7587) |
| `todo/tickets/do-block-tail-declaration-drops-its-trait.md` | [#7583](https://github.com/tokuhirom/mutsu/issues/7583) |
| `todo/tickets/doc-diff-harness-has-no-output-cap-or-nondeterminism-gate.md` | [#7590](https://github.com/tokuhirom/mutsu/issues/7590) |
| `todo/tickets/hyper-assign-to-a-list-of-lvalues-cannot-broadcast.md` | [#7586](https://github.com/tokuhirom/mutsu/issues/7586) |
| `todo/tickets/lazy-seq-argument-vanishes-into-a-user-slurpy.md` | [#7591](https://github.com/tokuhirom/mutsu/issues/7591) |
| `todo/tickets/nativecall-type-table-shadows-a-user-class-of-the-same-name.md` | [#7582](https://github.com/tokuhirom/mutsu/issues/7582) |
| `todo/tickets/nested-block-state-not-reset-in-value-position-for.md` | [#7585](https://github.com/tokuhirom/mutsu/issues/7585) |
| `todo/tickets/next-phaser-clobbers-value-position-for-result.md` | [#7592](https://github.com/tokuhirom/mutsu/issues/7592) |
| `todo/tickets/no-pseudostash-type-caller-stash-reports-stash.md` | [#7588](https://github.com/tokuhirom/mutsu/issues/7588) |
| `todo/tickets/object-hash-composite-key-increment-does-not-accumulate.md` | [#7538](https://github.com/tokuhirom/mutsu/issues/7538) |
| `todo/tickets/one-element-array-raku-omits-comma-for-subclass-element.md` | [#7594](https://github.com/tokuhirom/mutsu/issues/7594) |
| `todo/tickets/one-element-slice-assignment-rvalue-is-not-a-list.md` | [#7589](https://github.com/tokuhirom/mutsu/issues/7589) |
| `todo/tickets/regex-assertion-scalar-write-to-outer-lexical-is-lost.md` | [#7593](https://github.com/tokuhirom/mutsu/issues/7593) |
