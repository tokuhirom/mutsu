.PHONY: test roast check-roast-whitelist check-value-wall check-flaky-list

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
# `prove -e scripts/run-t-test.sh`: routes t/ through the same per-file timeout
# + flaky-quarantine wrapper the roast suite uses (docs/flaky-test-policy.md).
#
# `MUTSU_BIN=.../debug/mutsu`: run t/ on the DEBUG binary, matching CI's TAP
# step (ci.yml runs `prove t/` on target/debug/mutsu). The release build is
# reserved for `make roast`. Building release here too cost ~19 min (a full
# optimized recompile of the mutsu crate) for only a ~4 min t/ runtime saving
# vs debug — it dominated `make test` wall-clock for no correctness gain. See
# docs/adr/0014-make-test-runs-tap-on-debug-binary.md.
test: check-value-wall check-flaky-list
	@mkdir -p tmp
	(cargo build && cargo test -- --test-threads=1 && cargo test -p mutsu-lsp && MUTSU_BIN='$(CARGO_TARGET_DIR)/debug/mutsu' MUTSU_T_TIMEOUT=60 prove -e 'scripts/run-t-test.sh' t/) 2>&1 | tee tmp/make-test.log

check-value-wall:
	scripts/check-value-wall.sh

check-flaky-list:
	scripts/check-flaky-list.sh

roast:
	@mkdir -p tmp
	@rm -f temp-file-RT-126006-test
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -j$(PROVE_JOBS) -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
