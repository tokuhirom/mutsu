# An interpolating string argument of a regex assertion lowers

`<.panic("Modifier '{$/.Str}' not supported")>` in a token made the unit fail
to round-trip through RakuAST (`EVAL does not yet support lowering
RakuAST::QuotedString`), which stopped `use Polyglot::Regex::ECMA262::Grammar`.
A regex assertion keeps its argument list as source text, and the spelling of
a `"..."` with interpolated `{...}` blocks had no rendering for the block
segments. `regex_tree::expression_source` now spells a string-interpolation
part that is a code block back as `{stmt; stmt}`. Issue #15823.
