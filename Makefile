.PHONY: test roast check-roast-whitelist

test:
	cargo test
	cargo build
	prove -e 'target/debug/mutsu' t/

roast:
	cargo build
	prove -e 'timeout 30 target/debug/mutsu' $(shell cat roast-whitelist.txt)

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
