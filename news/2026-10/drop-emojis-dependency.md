# Emoji short names come from a committed table; `emojis` is now dev-only

`\c[...]` and `uniparse` resolve CLDR emoji short names (for example
`\c[family: man woman girl boy]`) through `src/builtins/emoji_name_table.rs`, a
generated table of normalized names (lowercase, commas removed) that is
binary-searched. The previous lookup walked all ~1900 entries of the `emojis`
crate and allocated two lowercased copies of each name on every call.

The `emojis` crate (and its private `phf 0.13` copy) is no longer a runtime
dependency. It stays as a dev-dependency: `builtins::emoji_name_gen` re-derives
the table from it on every test run and fails if the committed file has
drifted, and `MUTSU_UPDATE_EMOJI_TABLE=1 cargo test --lib emoji_name_gen &&
cargo fmt` regenerates it. A second test checks that every name resolves to the
same emoji the old first-match linear scan returned. (#10437)
