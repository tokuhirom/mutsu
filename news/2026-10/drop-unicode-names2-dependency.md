# Unicode character names come from a committed table; `unicode_names2` is now dev-only

`.uniname`, `uninames`, `\c[NAME]` and `uniparse` now resolve names through
`src/builtins/unicode_name.rs` over the generated, committed
`src/builtins/unicode_name_data.rs`: 40,470 stored names compressed as runs of
word tokens (17,983 distinct words), a codepoint-sorted index for
char -> name and an LM2-key-sorted index for the binary-searched name -> char
direction. CJK unified ideographs and Hangul syllables are computed, as the UCD
itself does, and the UAX #44 LM2 loose-matching rule (including the U+1180
HANGUL JUNGSEONG O-E exception) is implemented in-tree.

`unicode_names2` is no longer a runtime dependency, so a normal build no longer
compiles and runs its build script (`unicode_names2_generator`, `phf_codegen`,
`rand` and friends). It stays as a dev-dependency: `builtins::unicode_name_gen`
re-derives the tables from it on every test run and checks, for every
codepoint, that both directions answer exactly as the crate did.
Regenerate with `MUTSU_UPDATE_NAME_TABLE=1 cargo test --lib unicode_name_gen`.
(#10438)
