use Test;
use experimental :rakuast;

plan 15;

my $first = RakuAST::IntLiteral.new(7);
my $second = RakuAST::IntLiteral.new(19);
is $first.value, 7, 'literal accessor reads the first node';
is $second.value, 19, 'a cached row reads the current node';
is $first.value, 7, 'switching back does not cache field values';
is RakuAST::IntLiteral.^lookup('value').package.^name,
    'RakuAST::Literal', 'the inherited accessor belongs to Literal';

class ChildLiteral is RakuAST::IntLiteral {
    method new() { callwith(23) }
}
my $child = ChildLiteral.new;
is $child.value, 23, 'a user subclass reads its native payload';
is ChildLiteral.^lookup('value').package.^name,
    'RakuAST::Literal', 'a subclass retains the declaring owner';
is EVAL($first), 7, 'a hand-built literal still lowers and executes';
is EVAL($child), 23, 'a hand-built subclass still lowers and executes';

my $statement = Q[if 1 { 42 }].AST.statements[0];
isa-ok $statement.condition, RakuAST::IntLiteral,
    'parsed condition is read through an inherited accessor';
isa-ok $statement.then, RakuAST::Block, 'parsed branch has its measured field';
is RakuAST::Name.from-identifier('hello').canonicalize, 'hello',
    'derived accessors use the same row path';
is RakuAST::Postcircumfix::HashIndex.^can('assignee').elems, 0,
    'HashIndex does not advertise a nonexistent assignee';
is RakuAST::StatementPrefix::Phaser::Begin.^can('original-blorst').elems, 0,
    'BEGIN does not advertise the FIRST-only accessor';
ok RakuAST::StatementPrefix::Phaser::First.^can('original-blorst'),
    'FIRST retains original-blorst';
is $first.^can('missing-accessor').elems, 0, 'unknown names remain absent';
