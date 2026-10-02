use Test;

# From the SQL::Builder distribution: `our $*depth = CALLERS::<$*depth> // 0`
# in a routine that recurses through another one. The first call used to work
# and later calls saw no caller frame (a sub reading CALLERS:: skipped the
# frame push); re-declaring the same dynamic must also not store the caller's
# cell into itself (it hung).

plan 5;

sub inner { CALLERS::<$*p> // 'undef' }
sub outer { my $*p = 7; inner() }
is outer(), 7, 'first call';
is outer(), 7, 'second call';
is outer(), 7, 'third call';

sub b { our $*depth = CALLERS::<$*depth> // 0; $*depth }
sub f { $*depth++; b() }
sub g { our $*depth = 0; f() }
is g(), 1, 'redeclaring the dynamic from CALLERS:: sees the caller value';
is g(), 1, 'and again';

done-testing;
