use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 16;

sub code-var($source) {
    $source.AST.statements[1].expression.operand
}

my $plain = code-var(Q[my $p = "CORE"; &::($p)::uc("x")]);
is $plain.^name, 'RakuAST::Var::Package', 'the indirect routine is a package variable';
is $plain.sigil, '&', 'the code sigil is retained';
is $plain.name.parts[0].^name, 'RakuAST::Name::Part::Empty',
    'a root-qualified package has an empty leading part';
is $plain.name.parts[1].^name, 'RakuAST::Name::Part::Expression',
    'the package expression is kept as a name part';
is $plain.name.parts[1].expr.^name, 'RakuAST::Var::Lexical',
    'the package name reads its lexical variable';
is $plain.name.parts[2].name, 'uc', 'the routine name is a static part';

my $caller = code-var(Q[my $p = "CORE"; &CALLER::($p)::uc("x")]);
is $caller.^name, 'RakuAST::Var::Package', 'CALLER lookup has the same variable class';
is $caller.name.parts[0].name, 'CALLER', 'CALLER source provenance is retained';
is $caller.name.parts[1].^name, 'RakuAST::Name::Part::Expression',
    'CALLER lookup keeps the dynamic package part';
is $caller.name.parts[2].name, 'uc', 'CALLER lookup keeps the routine name';

is EVAL(Q[my $p = "CORE"; &::($p)::uc("x")].AST), 'X',
    'a root-qualified indirect call round-trips';
is EVAL(Q[my $p = "CORE"; &CALLER::($p)::uc("x")].AST), 'X',
    'a CALLER-qualified indirect call round-trips';

my $name = RakuAST::Name.new(
    RakuAST::Name::Part::Empty.new,
    RakuAST::Name::Part::Expression.new(RakuAST::StrLiteral.new('CORE')),
    RakuAST::Name::Part::Simple.new('uc'),
);
my $hand = RakuAST::Var::Package.new(name => $name, sigil => '&');
is EVAL($hand)('y'), 'Y', 'a hand-built root-qualified code variable lowers';

my $caller-name = RakuAST::Name.new(
    RakuAST::Name::Part::Simple.new('CALLER'),
    RakuAST::Name::Part::Expression.new(RakuAST::StrLiteral.new('CORE')),
    RakuAST::Name::Part::Simple.new('uc'),
);
my $caller-hand = RakuAST::Var::Package.new(name => $caller-name, sigil => '&');
is EVAL($caller-hand)('z'), 'Z', 'a hand-built CALLER code variable lowers';

is EVAL(Q[my $p = 'CORE'; &::($p)::uc].AST)('a'), 'A',
    'a routine reference round-trips without a call';
is EVAL(Q[my $p = 'CORE'; &CALLER::($p)::uc].AST)('b'), 'B',
    'a CALLER routine reference round-trips without a call';
