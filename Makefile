.PHONY: test lint roast check-roast-whitelist check-value-wall check-flaky-list check-t-layout

CARGO_TARGET_DIR ?= target
MUTSU_BIN ?= $(CARGO_TARGET_DIR)/release/mutsu

# Parallelism for the roast suite. CI runners (GitHub ubuntu-latest) have 4
# cores, so -j4 is the default. Going higher oversubscribes the CPU and makes
# the timing-sensitive S17 concurrency tests (scheduler/promise/supply) flake
# on their wall-clock assertions, so do not raise this above the core count.
# Override locally with `make roast PROVE_JOBS=8` if you know your box can take it.
PROVE_JOBS ?= 4

# `cargo test --test-threads=1`: the GC collector's `COLLECTING` flag is
# process-global, so a collect on one test thread trips
# `debug_assert!(!collecting())` inside an unrelated test running concurrently
# (observed twice on main as
# gc::gc_ptr::tests::arc_and_gc_strong_counts_stay_in_lockstep). The gc-stress
# CI job has serialized its `cargo test` for this reason since the GC landed;
# do the same everywhere instead of leaving the default-config runs to chance.
# Costs ~1s.
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
# the gc-stress / jit-stress CI jobs keep running the whole t/ suite on debug.
# See docs/adr/0075-make-test-runs-tap-on-release-binary.md, which supersedes
# ADR-0014.
test: check-value-wall check-flaky-list check-t-layout
	@mkdir -p tmp
	(cargo build --release && cargo test -- --test-threads=1 && cargo test -p mutsu-lsp && MUTSU_BIN='$(CARGO_TARGET_DIR)/release/mutsu' MUTSU_T_TIMEOUT=60 prove -r -e 'scripts/run-t-test.sh' t/) 2>&1 | tee tmp/make-test.log

# Every configuration mutsu ships, linted the way CI lints it. A warning only
# exists in the configuration you actually compile, so the default host build
# (the `test` job) misses both the Cranelift-less feature set the Miri job and
# the release fallback use, and the wasm32 lib the npm package is built from
# (the `lint-configs` job), plus rustdoc, whose intra-doc link resolution and
# Markdown parse no other configuration performs. Needs the wasm target:
#   rustup target add wasm32-unknown-unknown
lint:
	cargo clippy --workspace --all-targets -- -D warnings
	cargo clippy --no-default-features --features native --all-targets -- -D warnings
	cargo clippy --target wasm32-unknown-unknown --no-default-features --features wasm --lib -- -D warnings
	RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --document-private-items

check-value-wall:
	scripts/check-value-wall.sh

check-flaky-list:
	scripts/check-flaky-list.sh

check-t-layout:
	scripts/check-t-layout.sh

roast:
	@mkdir -p tmp
	@rm -f temp-file-RT-126006-test
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -j$(PROVE_JOBS) -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
