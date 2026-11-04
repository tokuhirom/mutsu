use Test;

# From the WhereList distribution: a failing attribute `where` throws a
# typed X::TypeCheck::Assignment (naming the anonymous refinement), not an
# untyped X::AdHoc.
plan 5;

class Scalar-Where { has $.bar where * > 2 }
try Scalar-Where.new(:bar(1));
isa-ok $!, X::TypeCheck::Assignment, 'scalar attribute where failure is typed';
is $!.message, 'Type check failed in assignment to $!bar; expected <anon> but got Int (1)',
    'the message names the anonymous refinement';

class Array-Where { has @.bar where { .all ~~ Int } }
try Array-Where.new(:bar<a b>);
isa-ok $!, X::TypeCheck, 'array attribute where failure is an X::TypeCheck';

lives-ok { Scalar-Where.new(:bar(3)) }, 'a passing where constructs';
is Scalar-Where.new(:bar(3)).bar, 3, 'and keeps the value';
