use Test;

# A Slip reifies on demand: `.Slip` and `slip(...)` of a genuinely lazy list
# (an infinite Range, a `.lazy` gather, a lazy `.map`) stay lazy instead of
# pulling the whole list (#13117).

plan 15;

sub inf { gather { my $i = 0; loop { take $i++ } } }

is-deeply inf.lazy.Slip.head(2), (0, 1), '.Slip of a .lazy gather';
is-deeply slip(1..*).head(3), (1, 2, 3), 'slip() of an infinite Range';
is (1..*).Slip.is-lazy, True, '.Slip of an infinite Range is lazy';
is-deeply (1..*).map(* + 1).Slip.head(2), (2, 3), '.Slip of a lazy .map';

my $s = (1..*).Slip;
is $s.^name, 'Slip', 'it is still a Slip';
is $s.gist, '(...)', 'it gists as a lazy list';
is $s[3], 4, 'it indexes lazily';
is-deeply $s.map(* + 1).head(2), (2, 3), 'it maps lazily';
is-deeply (0, |$s).head(3), (0, 1, 2), 'it slips into a list as a lazy tail';
is-deeply (0, slip(1..*)).head(3), (0, 1, 2), 'slip() in a list literal stays lazy';
is $s.List.is-lazy, True, '.List of it is lazy';
throws-like { $s.elems }, X::Cannot::Lazy, '.elems refuses a lazy Slip';

my @seen;
for slip(1..*) { last if $_ > 2; @seen.push: $_ }
is-deeply @seen, [1, 2], 'a for loop pulls it lazily';

is-deeply slip(gather { take 1; take 2 }).raku, 'slip(1, 2)', 'a finite gather still slips eagerly';
is-deeply (0, slip(1, 2)), (0, 1, 2), 'an eager slip still flattens';
