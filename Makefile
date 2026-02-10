.PHONY: test roast

test:
	cargo test
	prove -e 'cargo run --' t/

roast:
	prove -e 'cargo run --' $(shell cat roast-whitelist.txt)
