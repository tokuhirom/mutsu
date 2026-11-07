# `use RakuAST::Deparse::Highlight` works

Rakudo's own `RakuAST::Deparse::Highlight` (vendored verbatim, bundled in `Rakudo-Core`) loads and
`highlight($source, "DEBUG" | "Text" | "HTML")` gives the text rakudo 2026.09 gives, comments
included. What it needed from the interpreter:

- `nqp::gethllsym('Raku', 'Grammar' | 'Actions' | 'DEPARSE')` answer the `Raku::Grammar`,
  `Raku::Actions` and `RakuAST::Deparse` classes the module subclasses (the deparser is now also
  declared on demand, not only into a unit that mentions `DEPARSE`).
- `Str.AST(:compunit, :actions($obj))` reports the productions the source contains to the actions
  object: `comment:sym<#>` (the parser logs the line comments it skips), `statement-control:sym<use>`
  (with `<module-name>`), `statement-prefix:sym<BEGIN>`, `type-declarator:sym<constant>` and
  `doc-block:sym<finish>`, each with the character range rakudo reports, so the module's safe
  actions refuse `BEGIN`, `constant` and module loading as they do on rakudo (`typed-panic` throws
  the `X::Comp` form of the exception).
- `RakuAST::CompUnit.finish-content` (the text after `=finish`) and the `RakuAST::Pragma`
  class methods `IS-PRAGMA`, `IS-NYI`, `IS-ISM`, `KNOWN-PRAGMAS`, `KNOWN-ISMS`.

`t/rakuast/rakuast-deparse-highlight.t` gives identical results under `raku` and mutsu. Closes #15793
(the three ecosystem distributions that use it are re-checked in the PR).
