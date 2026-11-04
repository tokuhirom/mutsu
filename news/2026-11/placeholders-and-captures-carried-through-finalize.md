# Placeholder variables and positional captures are carried through the executing finalization

`$^a`, `@^l`, `%^h`, `&^cb`, `$:n` and `$0` were still converted to RakuAST and
lowered back by `finalize_unit`, although both directions give the same name
back: a placeholder declaration lowers to the caret- or colon-prefixed lexical
and `Var::PositionalCapture` to the canonical integer variable. Nothing the unit
declares can reclassify them, so `carried::context_free_name` now keeps them as
the parser built them (ADR-15760 slice 2). A code variable that is not a
placeholder (`&f`) is still classified by the unit's routines and converted.
