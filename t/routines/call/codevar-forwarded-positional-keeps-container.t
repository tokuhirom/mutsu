use Test;
plan 6;

# A positional argument handed to a pointy block stored in `&code`, which
# forwards it with `|c` to an `is raw` parameter, writes the caller's
# variable (#16179). Argument sources of the code-variable call are compiled
# like a plain sub call's, so the variable is tagged and an undefined one is
# not mistaken for a type name.
sub set7($d is raw) { $d = 7 }
my &wrap = -> |c { set7 |c };

my $undefined;
wrap $undefined;
is $undefined, 7, 'an undefined variable is written through |c';

my $defined = 1;
wrap $defined;
is $defined, 7, 'a defined variable is written through |c';

sub root($d is raw, &block) { $d = 8; block }
my &wrap2 = -> |c { root |c };
my $e;
wrap2 $e, { 1 };
is $e, 8, 'with a block argument beside it';

my $k = { $e };
my $g;
my &wrap3 = -> |c { root |c };
wrap3 $g, $k;
is $g, 8, 'with a stored block';

wrap2 my $declared, { 1 };
is $declared, 8, 'declared in the argument';

# The sub form keeps working.
sub fwd(|c) { set7 |c }
my $s;
fwd $s;
is $s, 7, 'sub form';
