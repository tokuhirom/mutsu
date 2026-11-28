use Test;

# #19344: a sequence generator that answers a Failure throws it when the
# sequence reaches that element, instead of caching the Failure.
plan 6;

sub NOSEQ { fail "not defined" }
my @a = 0, 1, 1, &NOSEQ ... *;

is-deeply @a[^3].List, (0, 1, 1), 'the elements before the Failure read fine';
throws-like { @a[5] }, X::AdHoc, :payload('not defined'), 'a single read past it throws';

my @b = 0, 1, 1, &NOSEQ ... *;
throws-like { @b[^6] }, X::AdHoc, :payload('not defined'), 'a slice reaching it throws';

my @c = 0, 1, 1, &NOSEQ ... *;
throws-like { @c[3..4] }, X::AdHoc, :payload('not defined'), 'a range slice throws';

my @d = 0, 1, 1, &NOSEQ ... *;
my $r = start { @d[^6] };
throws-like { await $r }, X::AdHoc, :payload('not defined'), 'in a thread too';

my @e = 1, 2, { $_ + 1 } ... 6;
is @e.join(','), '1,2,3,4,5,6', 'an ordinary generator is unchanged';
