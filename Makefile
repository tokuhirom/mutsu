.PHONY: test roast

test:
	cargo test
	cargo build
	prove -e 'target/debug/mutsu' t/

roast:
	cargo build
	prove -e 'timeout 30 target/debug/mutsu' $(shell cat roast-whitelist.txt)
