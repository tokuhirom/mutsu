use Test;
use experimental :rakuast;

plan 6;

# `RakuAST::Literal.from-value` builds a `ComplexLiteral` for a Complex and a
# `VersionLiteral` for a Version, as raku-doc/doc/Type/RakuAST/Literal.rakudoc
# documents and rakudo 2026.11 does (#18979).

is RakuAST::Literal.from-value(42+i).^name, 'RakuAST::ComplexLiteral', 'a Complex';
is RakuAST::Literal.from-value(v6.e).^name, 'RakuAST::VersionLiteral', 'a Version';
is RakuAST::Literal.from-value(42+i).DEPARSE, '<42+1i>', 'its DEPARSE';
is RakuAST::Literal.from-value(v6.e).DEPARSE, 'v6.e', 'its DEPARSE';
is-deeply RakuAST::Literal.from-value(42+i).EVAL, 42+i, 'EVAL gives the Complex back';
is RakuAST::Literal.from-value(v1.2.3).EVAL, v1.2.3, 'EVAL gives the Version back';
