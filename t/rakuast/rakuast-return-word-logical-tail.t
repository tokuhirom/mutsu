use experimental :rakuast;
use Test;

# `return X and Y` is `(return X) and Y`: the loose word-logical applies to the
# `return` call, whose right side never runs. Rakudo 2026.11 keeps the infix
# and its right operand in the tree (#18302); mutsu used to drop them.

plan 9;

sub body-of(Str $src) { $src.AST.statements[0].expression.body.statement-list.statements[0].expression }

is Q[sub f { return 1 and 2 }].AST.DEPARSE, "sub f \{\n    return 1 and 2\n}\n", '.DEPARSE keeps `and` and its operand';
is Q[sub g { return 1 or 2 }].AST.DEPARSE, "sub g \{\n    return 1 or 2\n}\n", '`or`';
is Q[sub h { return 1 xor 2 }].AST.DEPARSE, "sub h \{\n    return 1 xor 2\n}\n", '`xor` keeps its spelling';

my $infix = body-of(Q[sub f { return 1 and 2 }]);
isa-ok $infix, RakuAST::ApplyInfix, 'the statement is the infix';
isa-ok $infix.left, RakuAST::Call::Name::WithoutParentheses, 'over the return call';
is $infix.left.args.args.elems, 1, 'which takes the value';
is body-of(Q[sub k { return 1, 2 and 3 }]).left.args.args.elems, 2, 'a list is the call\'s arguments';

my @ran;
sub f { return 1 and @ran.push(1) }
sub g { return 0 or @ran.push(2) }
is-deeply (f(), g()), (1, 0), 'the routine returns the value';
is @ran.elems, 0, 'the right side never runs';
