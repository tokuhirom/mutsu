use v6;
use experimental :rakuast;
use Test;

# A sigilless `constant \term:<NAME>` is declared under its `term:<...>` name
# and referenced by the bare symbol, which renders as `Term::Name` of the
# declared name (measured against rakudo 2026.09).

plan 6;

my $ast = Q|{ constant \term:<ℵ₀> = Inf; ℵ₀ }|.AST;
my $gist = $ast.gist;
ok $gist.contains('RakuAST::VarDeclaration::Constant.new('), 'declaration is a Constant';
ok $gist.contains('name        => "term:<ℵ₀>"'), 'declared under the term:<> name';
ok $gist.contains('RakuAST::Term::Name.new('), 'the reference is a Term::Name';
ok $gist.contains('RakuAST::Name.from-identifier("term:<ℵ₀>")'), 'naming the declared term';

is EVAL($ast), Inf, 'EVAL of the AST lowers and runs';
is EVAL(Q|{ constant \term:<ℵ₁> = 7; ℵ₁ }|.AST), 7, 'the term constant has its value';
