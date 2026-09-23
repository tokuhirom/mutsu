# Digest::RIPEMD now handles multi-block inputs

mutsu now gives anonymous state in callback-created blocks a scope tied to the
closure instance. This fixes `Digest::RIPEMD` for multi-block inputs, including
the million-character test vector, and brings all four `Digest` 1.1.0 test files
to parity with Rakudo.
