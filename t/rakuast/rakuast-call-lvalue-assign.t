use Test;

# Assignments to a call result in RakuAST, measured on rakudo 2026.09: a
# plain `ApplyInfix(Assignment)` whose left side is the `Call::Name` (`f(1) =
# v`) or the `ApplyPostfix(Call::Term)` (`$c(2) = v`). An anonymous parameter
# targets the bare sigil. EVAL of the tree writes through the rw routine as
# the parsed program does.

plan 12;

my $named = Q[sub f($) is rw { my $x }; f(1) = 5].AST.statements[1].expression;
isa-ok $named.infix, RakuAST::Assignment, '`f(1) = 5` is an assignment';
isa-ok $named.left, RakuAST::Call::Name, 'to the routine call';
my $callable = Q[my $c; $c(2) = 6].AST.statements[1].expression;
isa-ok $callable.left, RakuAST::ApplyPostfix, '`$c(2) = 6` assigns to the applied call';
isa-ok $callable.left.postfix, RakuAST::Call::Term, 'a Call::Term';

my @p = Q[sub g($, @, %) { }].AST.statements.head.expression.signature.parameters;
is @p.map(*.target.name).join(' '), '$ @ %', 'anonymous parameters target the bare sigil';

sub run($src) { EVAL($src.AST) }
is run(Q[my $s = 0; sub cell() is rw { $s }; cell() = 7; $s]), 7, 'a routine lvalue is written';
is run(Q[my %h; sub slot($k) is rw { %h{$k} }; slot("a") = 1; slot("b") = 2; %h.sort.join(',')]),
    'a	1,b	2', 'with its arguments';
is run(Q[my $s = 0; sub cell() is rw { $s }; my $c = &cell; $c() = 11; $s]), 11,
    'a callable lvalue is written';
is run(Q[my $s = 0; sub cell() is rw { $s }; my $r = (cell() = 20); "$r $s"]), '20 20',
    'the assignment is an expression too';
is run(Q[my $s = 1; sub cell() is rw { $s }; cell() += 4; $s]), 5, 'a compound assignment still writes';
throws-like { run(Q[sub ro() { 1 }; ro() = 3]) }, X::Assignment::RO,
    'a routine that is not rw still refuses';
is run(Q[sub k($, @a, %) { @a.elems }; k(1, [2, 3], {})]), 2,
    'anonymous parameters still bind their positions';
