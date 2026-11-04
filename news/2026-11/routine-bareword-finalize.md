# Routine barewords are lowered by the classifier's answer

ADR-15760 slice 2: a plain-identifier word that the bareword classifier resolves to an
argument-less call (a routine the unit declares such as `die-on-fail`, or an imported
function) is no longer converted to a RakuAST `Call::Name::WithoutParentheses` node and
lowered back in `finalize_unit`. `bareword::call_word_expr` returns the word itself, which
is exactly what `lower_named_call` hands back for a source bareword, so the fallback no
longer runs for these words (a large share of the census's remaining `BareWord` entries).
