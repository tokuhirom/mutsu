use Test;

# A RakuAST node class constrains a variable like any type: `my RakuAST::Name $n`.
# The node classes are type objects, not registered classes (ADR-0011), and the
# declaration check reported "Type 'RakuAST::Name' is not declared. Malformed
# my". From CSS::Specification::Compiler, whose builders declare
# `my RakuAST::Name $name := $id.&name`. Expected values checked against raku.

plan 5;

my RakuAST::Name $x = RakuAST::Name.from-identifier('c');
is $x.DEPARSE, 'c', 'assignment to a RakuAST-typed variable';

my RakuAST::Name $bound := RakuAST::Name.from-identifier('b');
is $bound.DEPARSE, 'b', 'binding';

throws-like { $x = 1 }, X::TypeCheck::Assignment, 'a value of another type is refused';

my RakuAST::Name $empty;
is $empty.raku, 'RakuAST::Name', 'an uninitialised one holds the type object';

sub n(Str:D $id) {
    my RakuAST::StrLiteral $lit := RakuAST::StrLiteral.new($id);
    $lit.DEPARSE
}
is n('q'), '"q"', 'inside a routine';
