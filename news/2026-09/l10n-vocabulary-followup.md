# Complete L10N vocabulary parsing coverage

mutsu now applies generated L10N vocabulary entries at their parser seams:
localized infix operators, named arguments, quote and regex adverbs,
postcircumfix adverbs, and the `now`/`time`/`rand` terms are translated to the
canonical parser names. `use L10N::XX` activation is detected from the module's
`$*LANG.define_slang` AST call, so generated L10N distributions work without a
direct `use Slangify` declaration.

Metaoperator and quote-language entries remain intentionally unsupported; they
are reported as inert vocabulary residue rather than being given an incorrect
meaning.
