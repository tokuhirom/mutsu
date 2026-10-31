use Test;
use nqp;

# static::Analyzer reads package declarations, private methods and generic
# parameterized types at CHECK time (#15610). Expected values are rakudo
# 2026.09's.

plan 9;

class P { method !v() { 1 }; method v() { self!v() } }
sub g(::T $x, Array[T] $y) { 1 }

my (@classes, @methods);
my $generic;
CHECK {
    sub walk($n) {
        return unless $n ~~ RakuAST::Node;
        @classes.push: $n if $n ~~ RakuAST::Class;
        @methods.push: $n if $n ~~ RakuAST::Method;
        $n.visit-children({ walk($_) });
    }
    walk($*CU);
    my $sub = $*CU.statement-list.statements.grep(*.can("expression")).map(*.expression).grep(RakuAST::Sub)[0];
    $generic = $sub.compile-time-value.signature.params[1].type;
}

ok @classes[0].compile-time-value === P, 'a class declaration is its type object';
is @methods.elems, 2, 'both methods are in the tree';
my $private = @methods[0];
my $public = @methods[1];
ok $private.private, 'the private method is private';
ok $private.compile-time-value.signature.raku eq ':(P $:: *%_)', 'its value is the private method';
ok nqp::objectid($private.compile-time-value) == nqp::objectid(P.HOW.private_method_table(P)<v>), 'the same object the private table answers';
ok nqp::objectid($private.compile-time-value.candidates[0]) == nqp::objectid($private.compile-time-value), 'it is its own sole candidate';
is P.HOW.private_method_table(P)<v>.candidates.elems, 1, 'a public method of the same name is not a candidate';
ok nqp::objectid($public.compile-time-value) != nqp::objectid($private.compile-time-value), 'public and private differ';
ok $generic.^archetypes.generic, 'Array[T] is generic';
