use v6;
use Test;

# Rakudo wraps a plain `$` parameter in a read-only Scalar only when its
# nominal type could be Iterable (`lower_signature` in Perl6/Actions.nqp).
# A `Positional` / `Associative` typed `$` parameter binds its argument
# decontainerized, so it flattens where an untyped one stays one item.
# Found via Prettier::Table, whose `!stringify-hrule(Positional :$alignments?)`
# does `my @aligns = $alignments // ...` and drew a one-column header rule.
plan 17;

my @c = <l r>;
my $item = @c;

sub typed(Positional $x) { my @a = $x; @a.elems }
is typed(@c), 2, 'Positional $x: an @-variable argument flattens';
is typed([1, 2, 3]), 3, 'Positional $x: an Array literal flattens';
is typed($item), 2, 'Positional $x: an itemized argument is decontainerized';

sub typed-named(Positional :$x) { my @a = $x // 1; @a.elems }
is typed-named(x => @c), 2, 'Positional :$x flattens through //';
is typed-named(x => [1, 2, 3]), 3, 'Positional :$x with an Array literal';

sub smiley(Positional:D $x) { my @a = $x; @a.elems }
is smiley(@c), 2, 'Positional:D $x also binds without a Scalar';

sub iterate(Positional $x) { my $n = 0; for $x { $n++ }; $n }
is iterate([1, 2, 3]), 3, 'for $x iterates a Positional-typed parameter';

sub assoc(Associative $x) { my %h = $x; %h.elems }
is assoc({ a => 1, b => 2 }), 2, 'Associative $x binds the hash itself';

class Table {
    method rule(Positional :$alignments) { my @a = $alignments // 'c'; @a.elems }
    method cols(Positional $p) { my @a = $p; @a.elems }
}
is Table.new.rule(alignments => @c), 2, 'method named Positional param flattens';
is Table.new.cols(@c), 2, 'method positional Positional param flattens';

my $pointy = -> Positional $x { my @a = $x; @a.elems };
is $pointy([1, 2]), 2, 'pointy block Positional param flattens';

# Mutation through the parameter still reaches the caller's array.
sub pusher(Positional $x) { $x.push(9); $x.elems }
my @m = 1, 2;
is pusher(@m), 3, 'push through a Positional param';
is @m.elems, 3, 'the push reached the caller';

# Iterable-related (or absent) nominal types keep the Scalar wrapper.
sub untyped($x) { my @a = $x; @a.elems }
is untyped(@c), 1, 'untyped $x stays one item';
sub any-typed(Any $x) { my @a = $x; @a.elems }
is any-typed(@c), 1, 'Any $x stays one item';
sub list-typed(List $x) { my @a = $x; @a.elems }
is list-typed(@c), 1, 'List $x stays one item';
sub copied(Positional $x is copy) { my @a = $x; @a.elems }
is copied(@c), 1, 'Positional $x is copy is a fresh Scalar';
