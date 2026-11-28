use Test;

# The cycle collector leaves an edgeless instance (every attribute a string or
# number) out of its scan (`Trace::acyclic_at_collect`); such objects must
# survive a collect intact. (That a cycle closed later is still reclaimed is
# pinned by the collector's own unit test, since DESTROY timing after a
# collect request is not something rakudo guarantees.)

plan 2;

my $destroyed = 0;
class Node {
    has $.name;
    has $.next is rw;
    submethod DESTROY { $destroyed++ }
}

sub touch($n) { $n.name }

# Many edgeless objects, each survivor-dropped, then a collect: none is
# reclaimed while still referenced, and the program keeps every value.
my @keep = (^500).map: { Node.new(name => "n$_") };
touch($_) for @keep;
quietly $*VM.request-garbage-collection;
is @keep.map(*.name).join(',').chars, (^500).map({ "n$_" }).join(',').chars,
    'edgeless live objects survive a collect intact';
is $destroyed, 0, 'and none was destroyed';
