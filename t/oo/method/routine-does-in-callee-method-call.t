use Test;

plan 5;

# A role composed into a routine through another reference (`&code does R`
# inside a callee) is seen by a later method call on the routine, including
# an accessor whose name is also a built-in method (`.cache`).

my role M {
    has %.cache;
    has $.h;
    method set(\cache, \h) { %!cache := cache; $!h := h; self }
    method hello { 'hi' }
}
sub mem(&code) { my %C = z => 1; (&code does M).set(%C, 1) }

sub b($x) { $x }
mem(&b);
is &b.^name, 'Sub+{M}', 'the routine carries the role';
is &b.hello, 'hi', 'a role method';
is &b.h, 1, 'a role accessor';
is-deeply &b.cache, {z => 1}, 'an accessor named like a built-in method';
my $c = &b;
is-deeply $c.cache, {z => 1}, 'through another variable';
