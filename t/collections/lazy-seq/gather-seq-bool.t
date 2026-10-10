use Test;

# From Distribution::Extension::Updater (File::Find returns a gather Seq):
# a gather Seq is true iff its body produces a first element.

plan 5;

my $empty = gather { if 0 { take 1 } };
nok so($empty), 'empty gather is false';
ok so(gather { take 1 }), 'non-empty gather is true';
my $n = 0;
my $g = gather { $n++; take 1; $n++; take 2 };
ok so($g), 'boolifying pulls';
is $n, 1, 'only the first element was pulled';
is $g.elems, 2, 'the rest is still available';
