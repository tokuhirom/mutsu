use v6;
use Test;

# Sibling of closure-capture-hash-element-callback.t: `compile_expr_assign`
# (an assignment used as an EXPRESSION, e.g. parenthesized `($cb = sub {...})`)
# is a distinct compile site from the ordinary `Stmt::Assign` statement form,
# and did not mark a closure-literal RHS as escaping the way its
# `compile_assignment_rhs_for_target` sibling already did. A closure stored
# this way therefore compiled as non-escaping, so a captured-and-mutated free
# variable (even one mutated only in a branch that never runs) was never
# boxed into a shared cell -- its capture fell back to a plain-value snapshot
# that a same-named PARAMETER of whatever unrelated method later invokes it
# could shadow (#8663).
plan 2;

class Inner {
    method invoke(Str :$path!) {
        my $func = $*MUTSU_TEST_8663_CALLBACK;
        $func(self);
    }
}

class Outer {
    method resource(Str $member) {
        my Str $path = $member;
        $path = 'never' if False;
        my $cb;
        my $seen;
        ($cb = sub ($self) {
            $seen = $path;
        });
        my $*MUTSU_TEST_8663_CALLBACK = $cb;
        Inner.new.invoke(path => '');
        $seen;
    }
}

is Outer.new.resource('hello'), 'hello',
    'closure over an expression-context assign callback keeps its own captured lexical';

{
    my $path = 99;
    ok $path == 99, 'unrelated same-named sibling lexical';
}

done-testing;
