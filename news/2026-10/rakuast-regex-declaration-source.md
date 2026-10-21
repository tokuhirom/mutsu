# RakuAST retains regex declaration provenance

Named and anonymous `token`, `rule` and `regex` declarations keep their written
source across RakuAST conversion, lowering, closure capture and module caching.
Regex `.gist` and `.raku` can therefore retain separator whitespace, comments,
quote delimiters and signatures. The provenance is private model metadata and
does not change the public constructor rendering.

Execution still compiles the current public tree. As in Rakudo, editing an
ArgList can change what a regex matches while its declaration display continues
to show the original parsed source. Scope capture shares the declaration text
instead of copying it per evaluation.

RegexTree now uses the existing shared code-block scanner, including identifier
apostrophes and Unicode quotes. Main-slang whitespace and regex code use one
linear bracketed-comment primitive with constant auxiliary space, including
repeated and Unicode delimiters. The anonymous declaration and value-provenance
helpers are split into their own modules.

`t/rakuast/rakuast-regex-source-provenance.t` measures the contract against
Rakudo's RakuAST frontend, including AST edits and cold/warm module loading.
The older LTM regression retains all five assertions with current `$($x)`
interpolation syntax in place of obsolete `${x}`.
