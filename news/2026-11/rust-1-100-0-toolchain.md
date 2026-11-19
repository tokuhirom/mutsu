# Rust toolchain upgraded to 1.100.0

The pinned Rust toolchain moves from 1.99.0 to 1.100.0. Every pin is bumped together: the
`dtolnay/rust-toolchain` action SHAs in the CI, release, bench, stress and ecosystem-sweep
workflows, `rust-version` in both manifests, `.mise.toml`, the Dockerfile builder image, and the
version mentions in the README, site, docs and the `rustc-too-old` skill. The session-start hook
derives its wanted version from those pins, so remote containers pick up 1.100.0 automatically.
