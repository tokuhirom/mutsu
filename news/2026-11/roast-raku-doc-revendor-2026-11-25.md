# Re-vendored roast at 3821e592 and raku-doc at 50954c6a (2026-11-25)

`roast/` moved from `7639d080` (2026-11-11) to `3821e592` (2026-11-25), and `raku-doc/` from
`5d041ab6` (2026-11-07) to `50954c6a` (2026-11-25), ahead of the 2026-11-28 release.

The roast update touches two files, both already whitelisted, and both still pass completely:

- `S02-types/WHICH.t` drops `AST` and `X::TypeCheck::Splice` from its type lists, because rakudo
  removed those classes.
- `S04-declarations/smiley.t` adds a regression test for rakudo#6955: `use variables :D` still
  allows untyped variables (`plan 67`).

The raku-doc update removes `Type/AST.rakudoc` and `Type/X/TypeCheck/Splice.rakudoc` and drops
their mentions from `Type/Macro.rakudoc` and `type-graph.txt`. No roast file was added or removed,
so the whitelist and the Roast compatibility figure (1425 / 1425 spectest files) are unchanged.
mutsu still registers `X::TypeCheck::Splice`, as the rakudo 2026.11 release does.
