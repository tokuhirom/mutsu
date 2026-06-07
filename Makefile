.PHONY: test roast check-roast-whitelist

CARGO_TARGET_DIR ?= target
MUTSU_BIN ?= $(CARGO_TARGET_DIR)/release/mutsu

# Parallelism for the roast suite. CI runners (GitHub ubuntu-latest) have 4
# cores, so -j4 is the default. Going higher oversubscribes the CPU and makes
# the timing-sensitive S17 concurrency tests (scheduler/promise/supply) flake
# on their wall-clock assertions, so do not raise this above the core count.
# Override locally with `make roast PROVE_JOBS=8` if you know your box can take it.
PROVE_JOBS ?= 4

test:
	@mkdir -p tmp
	(cargo build && cargo test && cargo build --release && prove -e '$(MUTSU_BIN)' t/) 2>&1 | tee tmp/make-test.log

roast:
	@mkdir -p tmp
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -j$(PROVE_JOBS) -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
