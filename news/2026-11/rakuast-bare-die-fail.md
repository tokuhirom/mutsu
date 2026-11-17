# RakuAST: a bare `die` / `fail` keeps no arguments

A bare `die` or `fail` throws `$!` again. The parser supplies the `$!` as the
statement's argument, so RakuAST viewed `die` as `die $!` and deparsed it that
way. Rakudo's RakuAST frontend keeps a `Call::Name::WithoutParentheses` with
no arguments.

The parser now marks the supplied argument with `Spelling::ImplicitError`.
The view renders the call without arguments. Lowering a bare `die` / `fail`
supplies the same marked `$!`, so `EVAL` of the `.AST` still throws `$!`
again. Found through the `.DEPARSE` corpus (`S17-scheduler/at.t`, #14880).
