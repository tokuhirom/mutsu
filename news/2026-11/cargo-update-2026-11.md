# Cargo dependencies refreshed within their semver ranges (2026-11)

Before the 2026-11-28 release, `cargo update` moved 39 locked crates to the newest versions their
`Cargo.toml` requirements already allow. Notable moves: `cc` 1.3.0 -> 1.11.3, `smallvec` 1.15.2 ->
1.16.6, `log` 0.4.33 -> 0.4.35, `nix` 0.31.3 -> 0.31.4, `libffi-sys` 4.2.0 -> 4.2.2,
`cranelift-frontend` 0.137.1 -> 0.137.3, `regalloc2` 0.15.2 -> 0.15.3, `syn` 3.0.0 -> 3.0.10 and
`zerocopy` 0.8.54 -> 0.8.66. `synstructure` and `tinyvec_macros` dropped out of the graph, and
`utf_types` came in.

Only `Cargo.lock` changes. No requirement in `Cargo.toml` was raised, and no crate had to be held
back. Major bumps such as cranelift 0.138 are left for after the release.
