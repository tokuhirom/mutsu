# RakuAST: the `Signature` of an anonymous `sub` / `method` has its range

The parameters of an anonymous routine had their ranges already; the `Signature` node holding them
had none. It now runs from the first parameter to the end of the last, as in `raku` 2026.09
(`sub ($a, |c) { }`: 5-11). Measured over 12 shapes (typed, `where`, optional, default, slurpy, named,
sigilless, method literal, return type after the signature). The empty signature (`sub () { }`,
rakudo 5-5) and the body's `Blockoid` are not recorded by the parser. Test:
`t/rakuast/rakuast-anon-sub-signature-origins.t`.
