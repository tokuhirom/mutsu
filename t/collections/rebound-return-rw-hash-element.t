use Test;

plan 4;

# Rebinding a name onto one of its own hash elements must preserve the
# deferred element location across a routine boundary. This is the shape used
# by Text::Markov's graph walk.
my %graph;
sub slot() {
    my $p := %graph;
    $p := $p{'x'};
    return-rw $p;
}

my $s := slot();
$s //= BagHash.new;
$s{'foo'}++;
is %graph.raku, '{:x(("foo"=>1).BagHash)}',
    'return-rw of a rebound hash element keeps the live container';

# The same rebind must remain an ordinary writable scalar assignment when the
# returned element is not involved.
sub assign-through-rebound() {
    my %g;
    my $p := %g;
    $p := $p{'x'};
    $p = 42;
    %g.raku;
}
is assign-through-rebound(), '{:x(42)}',
    'plain assignment through a rebound hash element remains writable';

class C {
    has %!graph;

    method !slot() {
        my $p := %!graph;
        $p := $p{'x'};
        return-rw $p;
    }

    method put() {
        my $s := self!slot();
        $s //= BagHash.new;
        $s{'foo'}++;
    }

    method dump() { %!graph.raku }
}

my $c = C.new;
$c.put;
is $c.dump, '{:x(("foo"=>1).BagHash)}',
    'the method form writes through the rebound hash element';

my %inline;
my $inline := %inline;
$inline := $inline{'x'};
$inline //= BagHash.new;
$inline{'bar'}++;
is %inline.raku, '{:x(("bar"=>1).BagHash)}',
    'the inline rebound hash-element walk remains writable';
