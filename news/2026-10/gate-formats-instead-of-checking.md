# `scripts/dev gate` formats the tree instead of checking the format

The gate used to run `cargo fmt --all -- --check` as a blocking stage, so a file `rustfmt` could
fix in a second failed the whole gate and cost a rerun. `scripts/dev gate` now applies
`cargo fmt --all` to the working tree before it takes the tree id, and the `fmt` stage is gone from
both profiles. The result is still keyed by the formatted tree, the files the formatter rewrote are
named on stderr (commit them: the gate verifies the working tree, CI checks the pushed commit), and
a tree `rustfmt` cannot parse stops the gate before any job starts. `scripts/dev self-test` covers
`format_tree` on a throwaway crate. See the 2026-10-06 amendment to ADR-0126.
