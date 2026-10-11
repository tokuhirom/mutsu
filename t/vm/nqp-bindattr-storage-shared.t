use Test;
use nqp;

plan 6;

# nqp::bindattr of `$!storage` / `$!reified` onto an existing List/Map (#12672):
# the raw handle variable is rebound to the object's own node, so later writes
# through it are seen by the object.
my %r;
my $raw := nqp::hash;
nqp::bindattr(%r, Map, '$!storage', $raw);
nqp::bindkey($raw, 'k', 1);
is %r<k>, 1, 'a write through the raw hash handle is seen by the Map';

my @a;
my $l := nqp::list;
nqp::bindattr(@a, List, '$!reified', $l);
nqp::push($l, 7);
is @a.elems, 1, 'a push through the raw list handle is seen by the List';
is @a[0], 7, 'with the pushed element';

sub fill(%h) {
    my $r := nqp::hash;
    nqp::bindattr(%h, Map, '$!storage', $r);
    nqp::bindkey($r, 'z', 2);
    %h<z>
}
my %q;
is fill(%q), 2, 'the object sees it inside the routine';
is %q<z>, 2, 'and the caller sees it afterwards';

# The contents the raw handle already held move in.
my %s;
my $pre := nqp::hash;
nqp::bindkey($pre, 'a', 5);
nqp::bindattr(%s, Map, '$!storage', $pre);
is %s<a>, 5, 'the contents the handle held are installed';
