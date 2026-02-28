.PHONY: test roast check-roast-whitelist

CARGO_TARGET_DIR ?= target
MUTSU_BIN ?= $(CARGO_TARGET_DIR)/release/mutsu

test:
	cargo test
	cargo build --release
	prove -e '$(MUTSU_BIN)' t/

roast:
	cargo build --release
	prove -e 'timeout 30 $(MUTSU_BIN)' $(shell cat roast-whitelist.txt)

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
