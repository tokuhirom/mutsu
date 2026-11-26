# Four batteries refreshed to upstream HEAD

Four bundled libraries were behind upstream HEAD. They are now re-vendored
verbatim at their 2026-11-26 HEAD commits, and their `batteries.lock`
test-suite pins have moved with them (#19210, item 2):

- **Text::CSV** (`4e4ed415` → `0300d63c`): upstream's unreleased `main`
  after 0.022.
  - Signatures move from `returns T` to `--> T`.
  - New `column_indices`.
  - `fields` honours `column_names` when picking a `colrange` (Tux/CSV#56).
  - A fixed typo in error 1013.
- **XML** (`0349d282` → `0e4838a7`): `<namechar>` / `<namestartchar>`
  character ranges consolidated.
- **Cro::Core** (`cfabfbc8` → `3f4619f4`): `.perl` → `.raku` in `Cro.rakumod`.
- **NativeHelpers::Blob** (`cec91886` → `faefc4b5`): no change to a shipped
  file beyond the `Changes` header. Upstream added only a sponsor button.

The battery gate passes with every battery at 321/326 files. The five
non-passing files are the same non-whitelisted ones as before, so
`batteries-whitelist.txt` is unchanged. The Text::CSV smoke test now also pins
`column_indices`.
