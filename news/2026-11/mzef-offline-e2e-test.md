# An offline end-to-end test for the mzef install pipeline

`tests/mzef_e2e.rs` drives the vendored zef through a whole package lifecycle
with no network: `mzef install` of a local fixture distribution
(`tests/fixtures/mzef-dist`, whose test file `use`s the distribution itself)
with tests ON, `use` of the installed module, `mzef list --installed`,
`mzef uninstall`, and a final `use` that must fail. A second case installs a
distribution whose suite fails and checks that zef exits 1. The three bugs a
manual check found on 2026-11-27 (#19246, #19247, #19248) each fail it.
