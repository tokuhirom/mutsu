# Regenerate the emoji name table for emojis 0.9.1 (Unicode 18)

The `emojis` crate bump to 0.9.1 adds the Unicode 18 emoji set, so the committed CLDR
short-name table (`src/builtins/emoji_name_table.rs`) was missing names such as
`cracking face`. The table was regenerated with the documented
`MUTSU_UPDATE_EMOJI_TABLE=1 cargo test --lib emoji_name_gen && cargo fmt` procedure.
