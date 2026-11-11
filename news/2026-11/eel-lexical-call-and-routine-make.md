# A slang's call resolution sees lexical routines; `make` from a called routine

Two more pieces of EEL 0.3's test suite (#16888) now match rakudo.

A slang action that reads its arguments with the core `<arglist>` saw every
call to a core routine name resolved to the setting, even inside a block that
declares its own `sub any`. EEL rewrites a resolved `any(...)` into its own
junction pattern, so its "custom lexical any is not rewritten" check failed. The
RakuAST resolver now asks the enclosing scopes first, as it already did for
operators, and leaves a shadowed call unresolved against the setting.

A routine called from a regex code block can `make` on the block's `$/` — EEL's
`&make` finds it through `nqp::getlexcaller('$/')`. The write landed on the
Match object, but mutsu carried a block's `make` to the finished match only
through the block's own frame, so `('a' ~~ / a { f 17 } /).made` stayed `Nil`.
The code-block runner now also reads what the block's `$/` was made into.
