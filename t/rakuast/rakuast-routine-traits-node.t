use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# The written traits of a routine are a typed parser node (`ParsedStmt::RoutineTraits`)
# that opens the body: the `.AST` parse keeps it, an ordinary parse builds none.
sub names($node) { $node.traits.map({ .isa(RakuAST::Trait::Is) ?? .name.canonicalize !! .^name }).join(',') }

my $tree = Q[sub f() is export is rw returns Int { state $x = 1; $x }].AST;
is names($tree.statements[0].expression), 'export,rw,RakuAST::Trait::Returns', 'written order is kept';

# The hand-lowered routine keeps its record, so converting it again reads the same traits.
my &f = EVAL($tree);
is-deeply f(), 1, 'ordinary execution ignores the record';

my $again = Q[class C { method m() is rw handles <x> { 1 } }].AST;
is $again.statements[0].expression.body.body.statement-list.statements[0].expression.traits.elems, 2,
    'method traits include handles';

is EVAL(Q[sub g() returns Int is rw { 3 }; g()]), 3, 'an ordinary parse needs no record';
done-testing;
