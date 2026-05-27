.PHONY: test roast check-roast-whitelist

CARGO_TARGET_DIR ?= target
MUTSU_BIN ?= $(CARGO_TARGET_DIR)/release/mutsu

test:
	@mkdir -p tmp
	(cargo build && cargo test && cargo build --release && prove -e '$(MUTSU_BIN)' t/) 2>&1 | tee tmp/make-test.log

roast:
	@mkdir -p tmp
	(cargo build --release && MUTSU_BIN=$(MUTSU_BIN) prove -e 'scripts/run-roast-test.sh' $(shell cat roast-whitelist.txt)) 2>&1 | tee tmp/make-roast.log

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
