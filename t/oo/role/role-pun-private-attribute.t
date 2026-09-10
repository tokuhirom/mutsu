use v6;
use Test;

plan 9;

# Instantiating a role directly puns it to a class. Its attributes must live in
# the punned instance's own cell, because that is what a private access
# (`$!attr`) inside a role method reads and writes. mutsu used to keep them only
# as mixin markers, so `$!attr` threw
# "P6opaque: no such attribute '$!attr' ... when trying to get a value".

role R {
    has $.parent is rw;
    has $!hidden;
    method show-parent { $!parent }
    method stash($v) { $!hidden = $v; $!parent = $v * 2 }
    method peek { $!hidden }
}

my $r = R.new(:parent(42));
is $r.show-parent, 42, 'private read of a role attribute on a punned role';
is $r.parent, 42, 'public accessor agrees';

$r.stash(7);
is $r.peek, 7, 'a private-only role attribute round-trips through a method';
is $r.show-parent, 14, 'a private write is visible to a later private read';
is $r.parent, 14, 'a private write is visible through the public accessor';

# The seed must not resurrect itself over a written value: reading the accessor
# first, then writing, then reading again.
my $s = R.new(:parent(1));
is $s.parent, 1, 'accessor before any write';
$s.stash(5);
is $s.parent, 10, 'accessor after a private write does not serve the stale seed';

# Composing the role into a class must keep working.
class C does R { }
my $c = C.new(:parent(3));
$c.stash(4);
is $c.peek, 4, 'private role attribute through a composing class';
is $c.parent, 8, 'public role attribute through a composing class';
