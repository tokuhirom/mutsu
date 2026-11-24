use v6.e.PREVIEW;
use Test;

plan 7;

# 6.e `CALLERS::` searches every caller from the first outward, so a block or
# thunk the routine runs between itself and the lookup does not hide its
# caller's symbol (#18836). A found symbol that is not dynamic is a Failure of
# X::Symbol::NotDynamic.

sub s1 { my $l1 = 2; s0() }
sub s0 {
    my @r;
    @r.push: CALLERS::<$l1>.exception.^name;
    @r.push: (try CALLERS::<$l1>.exception.^name);
    @r.push: (do { CALLERS::<$l1> }).exception.^name;
    if 1 { @r.push: CALLERS::<$l1>.exception.^name }
    my &b = { @r.push: CALLERS::<$l1>.exception.^name };
    b();
    @r
}
my @r = s1();
is @r[0], 'X::Symbol::NotDynamic', 'directly';
is @r[1], 'X::Symbol::NotDynamic', 'from a try thunk';
is @r[2], 'X::Symbol::NotDynamic', 'from a do block';
is @r[3], 'X::Symbol::NotDynamic', 'from an if block';
is @r[4], 'X::Symbol::NotDynamic', 'from a called block';

# Two frames up.
sub t2 { my $l1 is dynamic = 3; t1() }
sub t1 { t0() }
sub t0 { CALLERS::<$l1> }
is t2(), 3, 'a dynamic lexical two callers up';

# The nearest caller that has the name answers.
sub u2 { my $l1 is dynamic = 3; u1() }
sub u1 { my $l1 = 9; u0() }
sub u0 { CALLERS::<$l1>.exception.^name }
is u2(), 'X::Symbol::NotDynamic', 'the nearest caller with the name answers';

