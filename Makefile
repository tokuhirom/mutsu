.PHONY: test roast check-roast-whitelist

test:
	cargo test
	cargo build --release
	prove -e 'target/release/mutsu' t/

roast:
	cargo build --release
	prove -e 'timeout 30 target/release/mutsu' $(shell cat roast-whitelist.txt)

check-roast-whitelist:
	LC_ALL=C sort -c roast-whitelist.txt
