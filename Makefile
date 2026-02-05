.PHONY: test

test:
	cargo test
	tools/prove_existing_roast.sh
