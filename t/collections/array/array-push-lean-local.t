use Test;

# `@a.push($v)` on a plain declared `@` local appends in place without the
# general push's by-name probes (#19294). Every shape that needs those probes
# still answers as before.

plan 13;

my @a;
@a.push($_) for 1..5;
is-deeply @a, [1, 2, 3, 4, 5], 'plain pushes in a loop';
is-deeply @a.push(6), [1, 2, 3, 4, 5, 6], 'push returns the array';

my $x = 5;
my @p;
@p.push($x);
$x = 6;
is-deeply @p, [5], 'the pushed element is a copy of the value';

my @b := @a;
@b.push(7);
is @a.elems, 7, 'a push through a := alias reaches the source';

my @t is default(0);
@t.push(Nil);
is-deeply @t, (my @ is default(0) = 0), 'Nil decays to the element default';

my Int @i;
@i.push(3);
throws-like { @i.push('x') }, X::TypeCheck, 'a typed array still checks its elements';

my @l := (1, 2);
throws-like { @l.push(3) }, X::Immutable, 'a List bound to an @ variable refuses';

my @s = <a b>;
@s.push('c', 'd');
is-deeply @s, [<a b c d>], 'a multi-value push';

my uint8 @u;
@u.push(300);
is @u[0], 44, 'a native integer array wraps';

my @r = 1, 2;
my @q;
@q.push(@r);
@r.push(3);
is @q[0].elems, 3, 'a reference push keeps sharing the source';

my @c;
my $f = { @c.push($_) };
$f($_) for 1..3;
is-deeply @c, [1, 2, 3], 'a push from a closure';

my @w = 1;
await start { @w.push(2) };
@w.push(3);
is @w.elems, 3, 'pushes from another thread and back';

my @e;
@e.push(Any);
is @e.elems, 1, 'a type object is pushed';
