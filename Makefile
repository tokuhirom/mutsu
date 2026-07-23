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
# `prove -e scripts/run-t-test.sh`: routes t/ through the same per-file timeout
# + flaky-quarantine wrapper the roast suite uses (docs/flaky-test-policy.md).
test: check-value-wall check-flaky-list
	@mkdir -p tmp
	(cargo build && cargo test -- --test-threads=1 && cargo build --release && MUTSU_BIN='$(MUTSU_BIN)' MUTSU_T_TIMEOUT=60 prove -e 'scripts/run-t-test.sh' t/) 2>&1 | tee tmp/make-test.log

check-value-wall:
	scripts/check-value-wall.sh

check-flaky-list:
	scripts/check-flaky-list.sh

roast:
	@mkdir -p tmp
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -j$(PROVE_JOBS) -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
