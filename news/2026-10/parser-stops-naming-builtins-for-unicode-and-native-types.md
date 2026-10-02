# The parser stops reaching into the builtins for Unicode tables and native type names

Second slice of issue #10779 (breaking the module cycles that keep the crate from being split).
The parser's upward references to the runtime and the builtins fell from 49 to 24, and the
lower layers' total (`make check-layer-deps`) from 192 to 165.

- **`src/ucd/`** is a new lower module for the Unicode Character Database lookups that need no
  runtime: the general-category table (`ucd::gc`, with its generated data and generator), the
  single-character numeric values (`ucd::numeric`: decimal digits of every `Nd` block, vulgar
  fractions, `Nl`/`No`/Unihan numerals) and NFC (`ucd::normalize::nfc`). The parser reads all
  three for numeric literals, identifiers and string literals. The shared table generator moved
  with them and now takes its output path relative to `src/`, so `unicode_script_gen` still
  writes into `src/builtins/`. Regenerating the GC table is now
  `MUTSU_UPDATE_GC_TABLE=1 cargo test --lib ucd::gc_gen`.
- **`src/native_types.rs`** (the `int8`/`uint64`/... name predicates and native-int wrapping)
  moved out of `src/runtime/`.
- **`src/term_names.rs`** holds the pure half of the sigil-less constant term namespace
  (`term_key`, `term_spelling`, `decl_storage_name`, ...). The `impl Interpreter` half that
  resolves a term against the env stays in `src/runtime/term_names.rs`.

`builtins` and `runtime` re-export everything under the old paths, so no other caller changed.
No behavior change.

What the parser still names upward needs more than a move: the type/enum/constant catalogs
(`is_known_type_constraint` consults the interpreter), `validate_regex_structurally`, `CType`,
`arith_div`, `parse_complex_str`, the slang activation and the module-export probes — the
last two are the essential compile-time calls that want a trait.
