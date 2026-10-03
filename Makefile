.PHONY: test lint roast checks check-roast-whitelist check-value-wall check-flaky-list check-t-layout check-magic-keys check-panic-surface check-pipefail check-bench-det check-prims check-dev check-interp-construction check-ast-walkers check-layer-deps check-interp-fields check-name-scans check-method-arms check-adr check-runner-pins check-integration-tests adr-index

# Recipes run under bash with `pipefail`, because the two suite recipes pipe
# into `tee` and POSIX sh reports only the *last* command's status -- `tee`'s,
# which is always 0. Without this, a failing `cargo build`, `cargo test` or
# `prove` made `make test` / `make roast` exit 0, i.e. the pre-publication gate
# AGENTS.md relies on reported success on a red suite (#8221). `check-pipefail`
# below is the guard that this stays true; it is a prerequisite of both targets.
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -c

CARGO_TARGET_DIR ?= target
MUTSU_BIN ?= $(CARGO_TARGET_DIR)/release/mutsu

# Parallelism for the roast suite. CI runners (GitHub ubuntu-24.04) have 4
# cores, so -j4 is the default. Going higher oversubscribes the CPU and makes
# the timing-sensitive S17 concurrency tests (scheduler/promise/supply) flake
# on their wall-clock assertions, so do not raise this above the core count.
# Override locally with `make roast PROVE_JOBS=8` if you know your box can take it.
PROVE_JOBS ?= 4

# `cargo test --test-threads=1`: the GC collector's `COLLECTING` flag is
# process-global, so a collect on one test thread trips
# `debug_assert!(!collecting())` inside an unrelated test running concurrently
# (observed twice on main as
# gc::gc_ptr::tests::arc_and_gc_strong_counts_stay_in_lockstep). Costs ~1s.
#
# `MUTSU_GC=on`: the unit-test build defaults the collector off only so that
# PARALLEL test threads do not cross-talk; serialized, the tests run under the
# configuration that ships, matching CI's `Unit tests` step (ADR-10738).
#
# `cargo test -p mutsu-lsp`: the language server is a separate workspace member
# (ADR-0065 D7), so the root `cargo test` -- which builds `default-members`, the
# `mutsu` package alone -- does not reach it. CI runs it as its own step; run it
# here too so `make test` still means the same thing locally.
#
# `prove -r -e scripts/run-t-test.sh`: routes t/ through the same per-file
# timeout + flaky-quarantine wrapper the roast suite uses
# (docs/flaky-test-policy.md). `-r` is what lets t/ be a nested tree rather
# than 3938 flat files -- prove does not descend without it, and it still
# matches *.t only, so the t/lib fixtures stay invisible. See
# docs/t-directory-layout.md.
#
# `MUTSU_BIN=.../release/mutsu`: run t/ on the RELEASE binary, the same one
# `make roast` uses, matching CI's TAP step. `cargo test` still builds and runs
# the Rust unit tests in debug, so `debug_assert!` keeps its coverage there, and
# CI's `debug-tap` job keeps running the whole t/ suite on debug.
# See docs/adr/0075-make-test-runs-tap-on-release-binary.md, which supersedes
# ADR-0014.
#
# `RUST_MIN_STACK=8388608`: the same 8 MiB test-thread stack CI's `Unit tests`
# step sets. Without it `cargo test` ran on Rust's 2 MiB default, and a debug
# test that parses the vendored `Test` module cold (after a rebuild invalidated
# the precompilation cache) overflowed locally while passing in CI
# (`named_call_intern_budget`, found by the first `scripts/dev gate` run).
test: checks
	@mkdir -p tmp
	(cargo build --release && RUST_MIN_STACK=8388608 MUTSU_GC=on cargo test -- --test-threads=1 && RUST_MIN_STACK=8388608 cargo test -p mutsu-lsp && MUTSU_BIN='$(CARGO_TARGET_DIR)/release/mutsu' MUTSU_T_TIMEOUT=60 prove -r -e 'scripts/run-t-test.sh' t/) 2>&1 | tee tmp/make-test.log

# The static guards: no build, seconds in total. `make test` depends on them,
# and `scripts/dev gate` runs them as its first stage (`checks`), ahead of fmt
# and lint, so a misplaced `t/` file or a ratchet overshoot fails the gate in
# seconds instead of after `make lint` and the release build.
checks: check-pipefail check-value-wall check-flaky-list check-t-layout check-magic-keys check-panic-surface check-name-scans check-method-arms check-interp-construction check-ast-walkers check-layer-deps check-interp-fields check-bench-det check-prims check-dev check-adr check-runner-pins check-integration-tests

# Every configuration mutsu ships, linted the way CI lints it. A warning only
# exists in the configuration you actually compile, so the default host build
# (the `test` job) misses both the Cranelift-less feature set the Miri job and
# the release fallback use, and the wasm32 lib the npm package is built from
# (the `lint-configs` job), the opt-in `alloc-stats` measurement build, plus rustdoc, whose intra-doc link resolution and
# Markdown parse no other configuration performs. The wasm32 pass runs only
# when the target is installed (`rustup target add wasm32-unknown-unknown`):
# it recompiles the whole dependency tree for another triple, which is the
# slowest step on a small box, and CI's `lint-configs` job always runs it.
lint:
	cargo clippy --workspace --all-targets -- -D warnings
	cargo clippy --no-default-features --features native --all-targets -- -D warnings
	@if rustup target list --installed 2>/dev/null | grep -qx wasm32-unknown-unknown; then \
	  echo "cargo clippy --target wasm32-unknown-unknown --no-default-features --features wasm --lib -- -D warnings"; \
	  cargo clippy --target wasm32-unknown-unknown --no-default-features --features wasm --lib -- -D warnings; \
	else \
	  echo "lint: wasm32-unknown-unknown target not installed; skipping the wasm32 clippy pass (CI lint-configs runs it)"; \
	fi
	cargo clippy --features alloc-stats --all-targets -- -D warnings
	RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --document-private-items

check-value-wall:
	scripts/check-value-wall.sh

check-flaky-list:
	scripts/check-flaky-list.sh --self-test
	scripts/check-flaky-list.sh

check-t-layout:
	scripts/check-t-layout.sh
	python3 scripts/migrate-t-layout.py --check

# Hand-built `format!("__mutsu_...::{name}")` metadata keys are banned (#8087).
# Build them with `MetaNs` (src/meta_ns.rs) instead, which memoizes the
# key per (namespace, name). This was a shrinking per-file ratchet while the
# 276 pre-existing sites were worked through; they are all converted now, so
# the baseline file is gone and any new site simply fails.
check-magic-keys:
	scripts/check-magic-keys.sh

# Ratchet on the panic-family (unwrap/expect/panic!/unreachable!/todo!/
# unimplemented!) and #[allow( surface in src/ (issue #8186). Both counts may
# go down, never up. Re-cut after a change that shifts either number:
#   scripts/check-panic-surface.py --update
check-panic-surface:
	python3 scripts/check-panic-surface.py --self-test
	python3 scripts/check-panic-surface.py

# Ratchet on run-time package-name string surgery (#8899): `format!("{pkg}::
# {name}")`, `== "GLOBAL"`, and `"::"` splitting/classification outside
# src/parser/ and src/compiler/. A qualified name is derived from two things
# the caller already holds, so it belongs in src/qualified.rs's memoizing
# constructor, built once per pair. All three counts may go down, never up.
# Re-cut after a change that shifts any of them:
#   scripts/check-name-scans.sh --update
check-name-scans:
	scripts/check-name-scans.sh --self-test
	scripts/check-name-scans.sh

# Ratchet on name-matching method dispatch arms (ADR-11276): `"name" => ...`
# arms in the pure cascades (src/builtins/methods_0arg/, methods_narg/) and the
# slow path (src/runtime/methods*). A built-in method is a row with a handler
# in src/builtins/method_table/; both counts may go down, never up. Re-cut
# after a migration slice lowers one:
#   scripts/check-method-arms.sh --update
check-method-arms:
	scripts/check-method-arms.sh --self-test
	scripts/check-method-arms.sh

# Ratchet on constructing an `Interpreter` (#10118, #10151): only process entry
# points, thread spawns, the parse-time module probes and a thread_local may
# build one; everything else runs code on the caller's interpreter. Per-file
# counts live in scripts/interp-construction-allowlist.txt and may go down,
# never up. Re-cut after removing a site:
#   scripts/check-interp-construction.py --update
check-interp-construction:
	python3 scripts/check-interp-construction.py --self-test
	python3 scripts/check-interp-construction.py

# Ratchet on hand-rolled recursive Stmt/Expr walkers (ADR-0137): an AST
# analysis implements `crate::ast_visit::Visit` instead. Per-file counts live in
# scripts/ast-walkers-baseline.txt and may go down, never up. Re-cut after
# porting a walker:
#   scripts/check-ast-walkers.py --update
check-ast-walkers:
	python3 scripts/check-ast-walkers.py --self-test
	python3 scripts/check-ast-walkers.py

# Ratchet on upward references from the lower layers (#10779): the AST,
# parser, Value, opcode, Env, GC and the name/key leaf modules may not name the
# runtime, VM, compiler, builtins or `Interpreter`. Each such edge is a module
# cycle, and the cycles keep the crate from being split. Per-file counts live
# in scripts/layer-deps-baseline.txt and may go down, never up. Re-cut after
# moving a helper down or routing a call through a trait:
#   scripts/check-layer-deps.py --update
check-layer-deps:
	python3 scripts/check-layer-deps.py --self-test
	python3 scripts/check-layer-deps.py

# Ratchet on the direct fields of `struct Interpreter` (ADR-10779 D4): their
# number, in scripts/interp-fields-baseline.txt, may go down, never up, and
# every field must belong to a subsystem (the SUBSYSTEMS rules in the script).
# New state goes into its subsystem's type. Re-cut after extracting fields:
#   scripts/interp-field-matrix.py --update
check-interp-fields:
	python3 scripts/interp-field-matrix.py --self-test
	python3 scripts/interp-field-matrix.py --check

# Ban on private copies of the Str primitives (ADR-0117). The nqp:: op tables,
# the VM's nqp path and TRIR's runtime must call src/builtins/str_prim/ -- the
# routine the matching Str method uses -- instead of walking, casing,
# normalizing or searching a string themselves. They used to keep their own
# codepoint-indexed copies, which drifted from the grapheme-indexed methods.
check-prims:
	scripts/check-prims.sh --self-test
	scripts/check-prims.sh

# The bench series' allocation counts are read out of callgrind's own output
# (#8959), and that parse fails by UNDERCOUNTING silently: an allocator whose
# name it does not resolve contributes nothing and the total stays plausible.
# The first version of it was 1,374 low on bench-hash and looked entirely
# reasonable. So the extractor is self-tested against a synthetic profile with a
# known answer, including the name-compression and recursion-suffix shapes that
# have actually been got wrong. Pure text processing: needs no valgrind, no
# binary, and runs in milliseconds.
check-bench-det:
	scripts/bench-det.sh --self-test

# The job runner every agent session uses for long jobs and the pre-publication
# gate (ADR-0126). Its self-test covers what a silent bug would cost most:
# lost-job detection, the per-name lock, the tree-id result cache, and the
# prove-summary parser plus known-env-failure matcher that decide the gate's
# verdict. Seconds, no build.
check-dev:
	scripts/dev self-test

# docs/adr/: a new ADR is numbered by its GitHub issue (sequential numbers
# collided between parallel PRs), and there is no hand-written index to
# conflict on -- `make adr-index` builds it. CI runs the check in the
# always-on `changes` job, because a docs-only PR skips `test-check`.
check-adr:
	scripts/adr.sh --self-test
	scripts/adr.sh check

# .github/workflows: every runner label names an OS version, never a floating
# `*-latest` alias that GitHub re-points to a new image under an unchanged
# commit. CI runs it in the always-on `changes` job, because a PR that edits
# only a non-ci.yml workflow is docs-only and skips `test-check`.
check-runner-pins:
	scripts/check-runner-pins.sh --self-test
	scripts/check-runner-pins.sh

# tests/: `autotests = false` builds the integration tests as two binaries whose
# roots declare every file as a module, so a file neither root declares would
# silently never run. See tests/integration.rs.
check-integration-tests:
	scripts/check-integration-tests.sh --self-test
	scripts/check-integration-tests.sh

adr-index:
	@scripts/adr.sh index

roast: check-pipefail
	@mkdir -p tmp
	@rm -f temp-file-RT-126006-test
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -j$(PROVE_JOBS) -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

# Guard for #8221. Both suite recipes end in `| tee tmp/make-*.log`, so their
# exit status is only meaningful while the recipe shell has `pipefail` set. This
# target fails if a false-in-the-pipeline is ever masked again -- e.g. because
# SHELL/.SHELLFLAGS above were reverted, or a shell without `pipefail` is in
# use. It runs no build and costs milliseconds, so both suites depend on it.
check-pipefail:
	@if (exit 1) 2>&1 | tee /dev/null; then \
		echo 'check-pipefail: FAILED -- a failing command in a `| tee` pipeline exits 0.' >&2; \
		echo '  The recipe shell is not running with pipefail, so `make test` and' >&2; \
		echo '  `make roast` would report success on a red suite (issue #8221).' >&2; \
		echo '  Restore `SHELL := /bin/bash` and `.SHELLFLAGS := -o pipefail -c`.' >&2; \
		exit 1; \
	fi

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
