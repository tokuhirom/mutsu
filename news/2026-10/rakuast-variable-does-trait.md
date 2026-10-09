# RakuAST: `my %h does Role` is a declaration with a `Trait::Does`

`my %h does R`, `my @a does R = 1, 2` and `my $x is default(3) does R` stopped at
a `SyntheticBlock` refusal under `MUTSU_RAKUAST=1` (ADR-10723, #7564): the parser
expands a `does` declaration into the declaration, an in-place mixin on the fresh
variable and, last, the initializer (mixing in first would lose the role when the
assignment replaces the value). RakuAST keeps one `VarDeclaration::Simple` with a
`Trait::Does` and the initializer after it.

`ast::var_does` builds and recognizes the parser's shape; the converter renders
the declaration with the role as its last trait (a parameterized role keeps its
arguments, an `is default(..)` trait stays in front), and lowering splits the
`Trait::Does` entries off, lowers the declaration plain and expands it again. The
`.AST` text is identical to rakudo's on every form measured.

Six `t/` files join the round-trip ratchet; `t/rakuast/rakuast-variable-does-trait.t`
pins the read direction, the `EVAL` direction and the semantics.
