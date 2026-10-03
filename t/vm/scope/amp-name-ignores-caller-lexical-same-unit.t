use v6;
use Test;

# `&g` in a routine names the `&g` visible where the routine was declared,
# never a same-named `my &g` of whoever calls it -- also when the caller is in
# the same compilation unit (#10997; the cross-unit form is #10638).

plan 14;

sub g() { 'sub' }

sub calls-amp() { &g() }
sub reads-amp() { &g.name }
sub reads-amp-in-closure() { (-> { &g() })() }

{
    my $depth = 0;
    my &g = -> { $depth++ < 3 ?? calls-amp() !! 'recursed' };
    is g(), 'sub', '&g() in a sub ignores the caller block\'s my &g';
}
{
    my &g = sub q() { };
    is reads-amp(), 'g', '&g as a value ignores it too';
    is reads-amp-in-closure(), 'sub', '... and so does a closure inside the sub';
}

sub lexical-caller() { my &g = -> { 'caller' }; calls-amp() }
is lexical-caller(), 'sub', 'a caller routine\'s my &g is not seen either';

my $block = { &g() };
{
    my &g = -> { 'blk' };
    is $block(), 'sub', 'a block declared outside the my &g ignores it';
}

# Every real lexical `&g` still wins over the sub.
{
    my &g = -> { 'own' };
    is &g(), 'own', 'a my &g in the reading scope';
    is (-> { &g() })(), 'own', '... read from a nested closure';
    is (1..2).map({ &g() }).join(','), 'own,own', '... read from a map block';
}
sub outer() { my &g = -> { 'outer' }; sub inner() { &g() }; inner() }
is outer(), 'outer', 'a nested sub sees its enclosing routine\'s my &g';
sub with-param(&g) { -> { &g() } }
is with-param(-> { 'param' })(), 'param', 'a &g parameter, read from a closure';
is (-> &g { &g() })(-> { 'pointy' }), 'pointy', 'a single &g pointy parameter';
is (-> &g { g() })(-> { 'pointy-bare' }), 'pointy-bare', '... called as g()';
role R[&g] { method m { &g() } }
is (1 but R[-> { 'role' }]).m, 'role', 'a role &g type parameter';
class C { my &g = -> { 'class-body' }; method m { &g() } }
is C.m, 'class-body', 'a class-body my &g read from a method';
