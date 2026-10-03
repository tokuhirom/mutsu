# Three parse gaps on the way to Pod::To::PDF

Pod::To::PDF's dependency chain (FontConfig, Pod::To::Cairo) stopped at parse
time on three forms Rakudo accepts:

- `use FontConfig::Defs :$FC-LIB, :$types, :enums;` — a variable colonpair
  names an import tag by its key, as Rakudo imports by the pair's key whatever
  its value. The parser stopped collecting tags at the first `:$`, so the later
  `:enums` was never applied and an imported enum key (`when FcTypeUnknown`)
  read as an undeclared routine.
- `module FcName is export { ... }` — an inline module declaration now takes
  `is` traits, and `is export` publishes the module to importers, as it does
  for a class.
- `my %opts .= &get-opts;` — a declaration's `.=` can name a routine to call
  as a method on the fresh variable, like the statement form already could.

Pod::To::PDF itself now gets as far as running `Font::FreeType`'s native setup.
That needs `Pointer[T].deref` from NativeCall, which is moving to the upstream
module (#11203). Rakudo cannot load the chain either here: its HarfBuzz and
Cairo versions disagree on `Cairo::Glyphs`.
