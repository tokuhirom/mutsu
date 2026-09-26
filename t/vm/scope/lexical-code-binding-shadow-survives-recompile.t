use v6;
use Test;

# A lexical `&f` (a `my &f` binding, a `&f` sub parameter, or a role's `&f`
# type parameter) shadows any routine named `f`, so a bare `f()` must reach
# the binding. Two places lost that:
#
# - A role's method bodies compile on a fresh compiler with no enclosing
#   scope, so the role's `&f` parameter was invisible and `f()` dispatched to
#   an outer `sub f` (issue #9513).
# - The inline `map`/`grep`/`first` path and sequence generators compile a
#   block's AST again on a fresh compiler, so `f()` inside such a block did
#   the same even when the block's own compiled chunk got it right.

plan 12;

sub h() { 70 }
sub d($x) { 70 }

role Q[&h] {
    method direct  { h() }
    method closure { my $c = { h() }; $c() }
    method mapped  { (1..2).map({ h() }).List }
    method !priv   { h() }
    method viapriv { self!priv }
    method own     { my &h = { 5 }; h() }
}

my $o = 1 but Q[{ 10 }];
is $o.direct,  10,       'role &-parameter shadows an outer sub in a method';
is $o.closure, 10,       '... inside a closure in the method';
is $o.mapped,  (10, 10), '... inside a map block in the method';
is $o.viapriv, 10,       '... in a private method';
is $o.own,     5,        'a my &h inside the method shadows the role parameter';

role R[&f] { method g { f() } }
is (1 but R[{ 8 }]).g, 8, 'role &-parameter reached before any outer &f';
my &f = { 7 };
is (1 but R[{ 9 }]).g, 9, 'a later outer my &f does not shadow the role parameter';

class C does R[{ 11 }] { }
is C.g, 11, 'a class composing the role sees its own argument';

{
    my &d = -> $x { $x * 2 };
    is (1..4).map({ d($_) }).List,        (2, 4, 6, 8), 'my &d shadows sub d in a map block';
    is (1..4).grep({ d($_) > 4 }).List,   (3, 4),       '... in a grep block';
    is (1, { d($_) } ... * > 10).List,    (1, 2, 4, 8, 16), '... in a sequence generator';
}

sub k(&d) { (1..2).map({ d(3) }).List }
is k(-> $x { $x + 1 }), (4, 4), 'a &d sub parameter shadows sub d in a map block';
