use Test;
use experimental :rakuast;

plan 6;

# A written `Nil` argument is not a bare `return` (#14845).
is 'sub f { return Nil }'.AST.DEPARSE.lines[1].trim, 'return Nil', 'return Nil keeps its argument';
is 'sub f { return }'.AST.DEPARSE.lines[1].trim, 'return', 'a bare return has none';
is 'sub f($x) { return Nil if $x; 5 }'.AST.DEPARSE.lines[1].trim, 'return Nil if $x;', 'with a statement modifier';

my $with = 'return Nil'.AST.find-nodes(RakuAST::Call::Name)[0];
is $with.args.args.elems, 1, 'return Nil has one argument node';
my $bare = 'return'.AST.find-nodes(RakuAST::Call::Name)[0];
is $bare.args.args.elems, 0, 'bare return has no argument node';

sub g($x) { return Nil if $x; 5 }
is-deeply (g(1), g(0)), (Nil, 5), 'return Nil still returns Nil at run time';
