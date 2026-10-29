use Test;

# The generated accessor of an `@`/`%` attribute is replayed from a per-layout
# lane after its first read. The replay must keep what the full path does for
# an aggregate: decontainerize unless `is rw`, and hand back a container that
# carries the attribute's declared element type. Every read here is repeated so
# that the later ones come from the lane.

plan 23;

class C {
    has %.h;
    has Int %.typed;
    has @.a;
    has Str @.tarr;
    has @.rw is rw;
    has $.s = 'scalar';
    has $.holds-array;
}

my $o = C.new(h => { a => 1 }, typed => { x => 5 }, a => [1, 2, 3], tarr => <p q>, rw => [7, 8], holds-array => [4, 5]);

for 1..3 {
    is $o.h<a>, 1, "untyped hash attribute (read $_)";
    is $o.typed<x>, 5, "typed hash attribute (read $_)";
    is $o.a.elems, 3, "untyped array attribute (read $_)";
}

is $o.typed.of.^name, 'Int', 'a typed hash keeps its element type on repeated reads';
is $o.tarr.of.^name, 'Str', 'a typed array keeps its element type on repeated reads';
is $o.s, 'scalar', 'a scalar attribute still reads from the lane';
is $o.holds-array.elems, 2, 'an array stored in a $ attribute';

# The accessor of a non-rw attribute hands back a copy-on-write view: writing
# through it must not change the attribute (checked against raku).
my %copy = $o.h;
%copy<b> = 2;
is $o.h.elems, 1, 'a copy made from a read does not write back';

# `is rw` hands the container itself back.
$o.rw.push(9);
is $o.rw.elems, 3, 'an is rw array accessor returns the stored container';
$o.rw.push(10);
is $o.rw.elems, 4, 'and keeps doing so on the next read';

# Containers stored after construction are seen by the next read.
my $p = C.new;
is $p.h.elems, 0, 'an unset hash attribute reads empty';
is $p.a.elems, 0, 'an unset array attribute reads empty';
is $p.typed.of.^name, 'Int', 'an unset typed hash still reports its element type';

class D is C { has %.extra }
my $d = D.new(typed => { y => 1 }, extra => { k => 'v' });
for 1..2 {
    is $d.typed.of.^name, 'Int', "an inherited typed hash accessor (read $_)";
}
is $d.extra<k>, 'v', 'a subclass attribute';
is $d.typed<y>, 1, 'and the inherited one reads its own instance';
