use Test;

# #14916 (accessor survey): `return-type` of the literal nodes, as on rakudo
# 2026.09. A `Literal` answers the type of the value it holds; a
# `QuotedString` and a `QuoteWordsAtom` answer `Str`; a node that declares no
# type (a variable, a call, a ternary) still answers `Mu`.

plan 15;

sub arg($code) { $code.AST.statements[0].expression.args.args[0] }

ok RakuAST::IntLiteral.new(1).return-type === Int, 'IntLiteral is Int';
ok RakuAST::IntLiteral.new(10**30).return-type === Int, 'a big IntLiteral is Int';
ok RakuAST::NumLiteral.new(1e0).return-type === Num, 'NumLiteral is Num';
ok RakuAST::StrLiteral.new('a').return-type === Str, 'StrLiteral is Str';
ok RakuAST::RatLiteral.new(1.5).return-type === Rat, 'RatLiteral is Rat';
ok RakuAST::ComplexLiteral.new(<1+2i>.Complex).return-type === Complex, 'ComplexLiteral is Complex';
ok RakuAST::VersionLiteral.new(v1.2).return-type === Version, 'VersionLiteral is Version';

ok arg('say 42').return-type === Int, 'a parsed integer';
ok arg('say "a"').return-type === Str, 'a parsed string';
ok arg('say 1.5').return-type === Rat, 'a parsed rational';
ok arg('say 1e3').return-type === Num, 'a parsed num';
ok arg('say v1.2').return-type === Version, 'a parsed version';

my $q = RakuAST::QuotedString.new(segments => (RakuAST::StrLiteral.new('a'),));
ok $q.return-type === Str, 'QuotedString is Str';
ok RakuAST::QuoteWordsAtom.new(RakuAST::StrLiteral.new('a')).return-type === Str,
    'QuoteWordsAtom is Str';
ok RakuAST::Var::Lexical.new('$x').return-type === Mu, 'a variable declares no type: Mu';
