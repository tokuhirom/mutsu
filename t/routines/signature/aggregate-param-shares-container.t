use Test;

# A plain `@x`/`%x` parameter binds the caller's container itself: element
# stores, `.push`, `:delete` and a whole-container `=` all reach the caller,
# and so does every other alias of that container. The parameters are named
# unlike the callers' variables, so a by-name lookup cannot find the caller's. The light call paths bind
# an untyped Array/Hash argument this way directly (#12917); every other
# argument shape keeps the general binder. Both must agree.

plan 33;

# Subs (positional-light path).
sub h-store(%p) { %p<k> = 1; %p<y>:delete }
sub h-assign(%p) { %p = (b => 2) }
sub a-push(@q) { @q.push(3); @q[0] = 9 }
sub a-assign(@q) { @q = (7, 8) }
sub both($n, @q, %p) { @q.push($n); %p{$n} = @q.elems }

my %h = y => 1;
my $h-alias = %h;
h-store(%h);
is-deeply %h, {k => 1}, 'sub: element store and :delete reach the caller';
is-deeply $h-alias, {k => 1}, 'sub: ...and every alias of the hash';

h-assign(%h);
is-deeply %h, {b => 2}, 'sub: whole-hash assignment reaches the caller';
is-deeply $h-alias, {b => 2}, 'sub: ...and every alias of the hash';

my @a = 1, 2;
my $a-alias = @a;
a-push(@a);
is-deeply @a, [9, 2, 3], 'sub: push and element store reach the caller';
a-assign(@a);
is-deeply @a, [7, 8], 'sub: whole-array assignment reaches the caller';
is-deeply $a-alias, [7, 8], 'sub: ...and every alias of the array';

my @b; my %c;
both($_, @b, %c) for 1..3;
is-deeply @b, [1, 2, 3], 'sub: mixed scalar/aggregate params, repeated calls';
is-deeply %c, {'1' => 1, '2' => 2, '3' => 3}, 'sub: the hash param too';

sub itemized(@a) { @a.elems }
my $list = [1, 2, 3];
is itemized($list), 3, 'sub: an itemized Array binds as its elements';
sub h-elems(%h) { %h.elems }
is h-elems({a => 1, b => 2}), 2, 'sub: a literal hash binds';

# Shapes that keep the general binder still bind correctly.
sub seq-arg(@a) { @a.join(',') }
is seq-arg((1..3).map(* * 2)), '2,4,6', 'sub: a Seq binds through PositionalBindFailover';
sub pair-arg(%h) { %h.^name }
is pair-arg((a => 1)), 'Pair', 'sub: a Pair binds as itself';
my Int %typed;
sub typed-store(%h) { %h<x> = 'no' }
dies-ok { typed-store(%typed) }, "sub: a typed hash keeps its value type";
sub typed-assign(%p) { %p = (a => 'b') }
throws-like { typed-assign(%typed) }, X::TypeCheck::Assignment,
    'sub: whole assignment to a typed hash is still checked';
my Int @typed-a;
sub typed-push(@q) { @q.push('s') }
throws-like { typed-push(@typed-a) }, X::TypeCheck::Assignment,
    'sub: a typed array keeps its element type';
{
    # An outer same-named typed variable does not constrain the parameter.
    my Int %p;
    my Int @q;
    sub shadow-h(%p) { %p = (a => 'x'); %p<b> = 'y'; %p }
    sub shadow-a(@q) { @q = ('s',); @q.push('t'); @q }
    my %free; my @free;
    is-deeply shadow-h(%free), {a => 'x', b => 'y'}, 'sub: no constraint leaks into %p';
    is-deeply shadow-a(@free), ['s', 't'], 'sub: no constraint leaks into @q';
}
# The container descriptor name (`.VAR.name`) is the caller's variable, or
# "element" for a fresh (literal) container.
sub var-name(@q) { @q.VAR.name }
my @named-arr = 1, 2;
is var-name(@named-arr), '@named-arr', 'sub: .VAR.name is the caller variable';
is var-name([1, 2]), 'element', 'sub: .VAR.name of a literal is "element"';
class VN { method name-of(%p) { %p.VAR.name }; method arr-name-of(@q) { @q.VAR.name } }
my %named-hash = a => 1;
is VN.name-of(%named-hash), '%named-hash', 'method: .VAR.name is the caller variable';
is VN.arr-name-of([1]), 'element', 'method: .VAR.name of a literal is "element"';
# An object hash keeps keying by the object through a parameter.
class KeyA {}
class OH { method bump(%s) { %s{KeyA}++; %s.elems } }
my %object-hash{Mu};
OH.bump(%object-hash) for ^2;
is %object-hash{KeyA}, 2, 'method: an object-hash parameter stores by object';
sub oh-bump(%p) { %p{KeyA}++ }
my %object-hash2{Mu};
oh-bump(%object-hash2) for ^3;
is %object-hash2{KeyA}, 3, 'sub: an object-hash parameter stores by object';
my $not-positional = 42;
throws-like { a-push($not-positional) }, X::TypeCheck::Binding::Parameter, 'sub: a non-Positional is rejected';

# Methods (fast method binder).
class C {
    method h-store(%p) { %p<k> = 1 }
    method h-assign(%p) { %p = (z => 26) }
    method a-push(@q) { @q.push(5) }
    method conc($s, $n, %a, %c) {
        %a{$s} = $n;
        %c.elems
    }
}

my %m;
my $m-alias = %m;
C.h-store(%m);
is-deeply %m, {k => 1}, 'method: element store reaches the caller';
C.h-assign(%m);
is-deeply $m-alias, {z => 26}, 'method: whole-hash assignment reaches every alias';

my @m = 1;
C.new.a-push(@m);
is-deeply @m, [1, 5], 'method: push reaches the caller';

my %anchors; my %callbacks = x => 1;
is C.new.conc('a', 1, %anchors, %callbacks), 1, 'method: four-parameter call';
is-deeply %anchors, {a => 1}, 'method: the hash parameter was shared';

my Int @typed-arr;
class D { method push-str(@q) { @q.push('s') }; method j(@q) { @q.join(',') } }
is D.j((1..3).map(* + 1)), '2,3,4', 'method: a Seq binds through PositionalBindFailover';
class E { method store(%p) { %p<x> = 'no' } }
throws-like { E.store(%typed) }, X::TypeCheck::Assignment,
    'method: a typed hash keeps its value type';
dies-ok { D.push-str(@typed-arr) }, 'method: a typed array keeps its element type';
