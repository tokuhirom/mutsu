# `make lint` skips the wasm32 pass when the target is missing

`make lint` used to die on any box without `wasm32-unknown-unknown` installed,
and installing it meant recompiling the whole dependency tree for a second
triple — the slowest step of the local gate on a small remote container. The
wasm32 clippy pass now runs only when the target is installed and otherwise
prints a skip message. CI's `lint-configs` job still runs it on every PR, so
a wasm-only warning is caught before merge and fixed forward.
