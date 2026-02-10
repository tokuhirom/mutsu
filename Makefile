.PHONY: test

test:
	cargo test
	prove -e 'cargo run --' t/
