# `MY::` in a nested block lists only that block's routines

`MY::` inside a nested block used to list every routine visible anywhere in the
compilation unit: file-scope subs and everything an outer `use Test` imported
leaked into each inner block, so `{ MY::<&plan> }` answered the outer import
where Rakudo answers `Nil` (#10626).

The compiler now records which kind of pad a `MY::`/`LEXICAL::` names
(`OpCode::GetLexicalStash`'s `LexicalStashRoutines`). A compunit or routine root
keeps the old behavior; a nested block lists only the routines it declares
itself (tracked per scope frame by the compiler) and the routines its own `use`
statements imported — read from the block's run-time import scope, which now
records each import made while it is innermost, re-imports of an outer alias
included.

This makes the `from` distribution's `t/01-basic.rakutest` pass all nine tests
(it checks that a block's `use from "Test"` imports nothing into it).
