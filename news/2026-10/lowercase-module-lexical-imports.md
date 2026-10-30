# A lowercase-named module's imports are lexical like any module's

The compiler treated every lowercase-initial `use` as a pragma, so `use has-word` (a real
module, imported through a `sub EXPORT` hook) opened no lexical import set. An `EVAL`, or a
RakuAST node's `.EVAL`, inside the importing module's routines then died with "Undeclared
routine: has-word" on a cold cache (a precompiled load hid it). Only the pragmas mutsu
applies itself (`is_known_pragma_name`, `lib`) are exempt now. Needle::Compile's
`t/04-multiple.rakutest` passes; its `t/02-single.rakutest` still needs the module-private
type constant fix tracked in a separate issue.
