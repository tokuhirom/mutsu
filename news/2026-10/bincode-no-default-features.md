# bincode: drop unused default features

`bincode` is only used through its serde bridge (`precomp` and `scan_cache`), so the dependency now
sets `default-features = false, features = ["std", "serde"]`. The unused `derive` feature, and with
it the `bincode_derive` and `virtue` build-time crates, leave `Cargo.lock`. The precomp round-trip
tests pass unchanged.
