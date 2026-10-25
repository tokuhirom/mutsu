use Test;

# ADR-10723 Stage 2 (#14948): a statement-level `TARGET = SOURCE` whose target is
# a method call, a call, a callable or a `*`-slurpy list is the typed assignment
# node. The tree matches rakudo 2026.09 and runs like the parsed program.

plan 12;

sub assignment(Str $src) { $src.AST.statements[*-1].expression }

my $m = assignment(Q[class Cm { has $.a is rw }; my Cm $o .= new; $o.a = 5]);
is $m.left.^name, 'RakuAST::ApplyPostfix', 'a method-call target';
is $m.infix.raku, 'RakuAST::Assignment.new', 'plain assignment';

is assignment(Q[my @a; (@a, *) = 1, 2, 3]).left.^name, 'RakuAST::Circumfix::Parentheses',
    'a list target with a slurpy *';
is assignment(Q[my @a; (*, @a) = 1, 2, 3]).left.^name, 'RakuAST::Circumfix::Parentheses',
    'a leading slurpy *';
is assignment(Q[sub f is rw { my $x }; f() = 3]).left.^name, 'RakuAST::Call::Name',
    'a call target';
is assignment(Q[my $c = sub { my $z }; $c() = 1]).left.^name, 'RakuAST::ApplyPostfix',
    'a callable target';

is EVAL(Q[my @a; (@a, *) = 1, 2, 3; @a.raku].AST), '[1, 2, 3]', 'trailing * keeps the array';
is EVAL(Q[my @a; (*, @a) = 1, 2, 3; @a.raku].AST), '[2, 3]', 'leading * skips one';
is EVAL(Q[class Cr { has $.a is rw }; my Cr $o .= new; $o.a = 5; $o.a].AST), 5,
    'a method-call assignment writes through the accessor';
is EVAL(Q[my $v; sub g is rw { $v }; g() = 7; $v].AST), 7, 'a call assignment writes through';
is EVAL(Q[my $w; my $c = sub is rw { $w }; $c() = 8; $w].AST), 8, 'a callable assignment writes through';

my $ast = Q[my @a; (@a, *) = 1, 2, 3].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
