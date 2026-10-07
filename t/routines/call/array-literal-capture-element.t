use Test;

# From SQL::Builder t/10-where.rakutest: `where([\(:or[...])])` lost its
# only element because the single-element array literal flattened the
# Capture into its (empty) positionals.
plan 8;

my $a = [\(:a(1))];
is $a.elems, 1, 'array of one named-only Capture has one element';
isa-ok $a[0], Capture, 'the element is the Capture';
is $a[0]<a>, 1, 'its named arg is intact';

my $b = [\(:or[:bar(4), :bar(5)])];
is $b.elems, 1, 'Capture with a bracketed named value';
isa-ok $b[0], Capture, 'still a Capture';

my $c = [\(1)];
is $c.elems, 1, 'positional Capture stays one element';
isa-ok $c[0], Capture, 'positional Capture is not flattened';

sub f(+@c) { @c.elems }
is f([\(:x[1])]), 1, 'slurpy +@c sees the Capture';
