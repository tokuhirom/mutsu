use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# An assignment in expression position keeps its target as written, so `.AST`
# matches rakudo 2026.09, and the tree lowers to what the parsed program does
# (ADR-10723 Stage 2, #14948). This file passes under both mutsu and raku.

plan 12;

sub arg(Str $src) { $src.AST.statements[*-1].expression.args.args[0] }

my $grouped = arg(Q[my $x; say(($x) = 1)]);
is $grouped.left.^name, 'RakuAST::Circumfix::Parentheses', 'a parenthesised target keeps its parentheses';
is $grouped.infix.raku, 'RakuAST::Assignment.new', 'which makes it a list assignment';

is arg(Q[my $x; say($x = 5)]).infix.raku, 'RakuAST::Assignment.new(:item)', 'a scalar target is an item assignment';
is arg(Q[class C { has $.foo is rw }; my $o = C.new; say($o.foo = 1)]).left.^name, 'RakuAST::ApplyPostfix',
    'a method target is the method call';
is arg(Q[my @a; say(@a[0] = 1)]).postfix.assignee.^name, 'RakuAST::IntLiteral',
    'a positional subscript takes the source as its assignee';

my $nqp = Q[use nqp; my @i; nqp::atposref_i(@i, 0) = 5].AST.statements[*-1].expression;
is $nqp.left.^name, 'RakuAST::Nqp', 'an nqp op target is the op, without parentheses';

# The trees run like the parsed program.
is EVAL(Q[my $x; my $y = $x = 5; "$x $y"].AST), '5 5', 'a chained assignment';
is EVAL(Q[my $x; say(($x) = 7) for (); $x = 3; $x].AST), 3, 'a grouped scalar target';
is EVAL(Q[my @a; (@a[0] = 1).Str ~ @a.elems].AST), '11', 'a subscript target';
is EVAL(Q[class D { has $.foo is rw }; my $o = D.new; ($o.foo = 4); $o.foo].AST), 4, 'a method target';
is EVAL(Q[my $x; 1 andthen $x = 2; $x].AST), 2, 'an assignment under andthen';
is EVAL(Q[use nqp; my int @i = 1, 2; nqp::atposref_i(@i, 0) = 5; @i.join(",")].AST), '5,2',
    'an nqp op target writes through';
