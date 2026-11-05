# Public-attribute variables are carried through finalization

ADR-15760 slice 2: the public-attribute variables `$.n`, `@.a` and `%.h` read back as the same name
after the RakuAST round trip (`Var::Attribute::Public`), so `finalize_unit` keeps them as the parser
built them instead of converting each to a variable node and lowering it back. About a fifth of the
fallback expressions in a census over every 12th `t/*/*.t` (25 of 127) were these.
