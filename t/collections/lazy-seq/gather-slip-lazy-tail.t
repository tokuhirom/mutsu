use Test;

# The Slip of a plain gather reifies from the gather on demand, as Rakudo's
# Slip does, so slipping an infinite gather no longer hangs (#13117). A gather
# that ends still gives an eager Slip of its elements. Expected values are the
# `raku` (v2026.09) output.

plan 11;

sub inf { gather { my $i = 0; loop { take $i++ } } }

is slip(inf).head(3), (0, 1, 2), 'slip() of an infinite gather reads a prefix';
is inf.Slip.head(3), (0, 1, 2), '.Slip of an infinite gather reads a prefix';
is inf.lazy.Slip.head(2), (0, 1), '.Slip of a lazy gather reads a prefix';
is slip(1..*).head(3), (1, 2, 3), 'slip() of an infinite Range reads a prefix';
ok (1..*).Slip.is-lazy, '.Slip of an infinite Range is lazy';
is (1, slip(inf)).head(3), (1, 0, 1), 'a list literal reads into a slipped infinite gather';

is slip(gather { take 1; take 2 }).raku, 'slip(1, 2)', 'a finite gather slips its elements';
is (gather { take 1 }).Slip.raku, 'slip(1,)', '... and so does its .Slip';
my @a = slip(gather { take 1; take 2 });
is @a.raku, '[1, 2]', 'an array assigned a finite gather Slip holds its elements';
is (1, 2).map({ slip(gather { take $_; take $_ }) }).raku, '(1, 1, 2, 2).Seq',
    'a map callback returning a finite gather Slip flattens it';

my $h = gather { take 1; take 2 };
my $t = slip($h);
is $h.raku, '$((1, 2).Seq)', 'slip() leaves the gather readable';
