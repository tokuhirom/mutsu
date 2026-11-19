# Rust toolchain upgraded to 1.100.0

The pinned Rust toolchain moves from 1.99.0 to 1.100.0. Every pin is bumped together: the
`dtolnay/rust-toolchain` action SHAs in the CI, release, bench, stress and ecosystem-sweep
workflows, `rust-version` in both manifests, `.mise.toml`, the Dockerfile builder image, and the
version mentions in the README, site, docs and the `rustc-too-old` skill. The session-start hook
derives its wanted version from those pins, so remote containers pick up 1.100.0 automatically.

Fallout fixed in the same change: six new `clippy` findings (a duplicated `#[allow]`, four
`needless_bool` guards, one `needless_range_loop`), one rustfmt reflow, and the bundled-module
lookup for `#[test]` binaries. Cargo 1.100 builds the unit-test executable under
`target/<profile>/build/<pkg>/<hash>/out/` instead of `deps/`, so the old relative `modules/`
candidates missed and every `use Test` inside a `#[test]` failed with "Could not find Test". A
new candidate, taken only for a path of the form `<root>/target/<profile>/build/..`, restores it for
unit and integration tests; installed binaries search exactly the same locations as before.

The Miri job's pinned nightly moves from `nightly-2026-08-01` (rustc 1.99) to `nightly-2026-09-01`
(rustc 1.100.0-nightly), because the new `rust-version` makes cargo refuse the older nightly. The
wasm32 lint pass also flagged one `needless_bool` that only exists with `cfg(unix)` off.
