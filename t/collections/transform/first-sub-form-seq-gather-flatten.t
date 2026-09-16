use Test;

# Sub-form `first(&matcher, @list)` must flatten a `Seq`/`Hash`/`Slip` list
# argument the same way `map`/`grep` already do, and must route a
# gather-sourced (lazy-pipe) `Seq` through the method form instead of
# treating the whole pipe as a single opaque item.
#
# Found while investigating why `mzef install` crashed on every distribution:
# vendored zef's `Zef::CLI` picks an install target with
#   state $cur = first { .can-install() }, map { ... }, <site home>;
# The `map { ... }, <site home>` sub-call produces an eager `Seq` of two
# `CompUnit::Repository` candidates. `first` pushed that whole `Seq` as one
# list item instead of iterating it, so `.can-install()` ran on the `Seq`
# itself and mutsu died with "No such method 'can-install' for invocant of
# type 'Seq'" before ever resolving a single dependency.

plan 8;

class HasCanInstall {
    has $.ok;
    method can-install() { $!ok }
}

# --- the exact zef shape: first over a Seq built by a sub-form map ---
{
    my @list = HasCanInstall.new(:ok(False)), HasCanInstall.new(:ok(True));
    my $seq = map { $_ }, @list;
    isa-ok $seq, Seq, 'sanity: sub-form map over a list returns a Seq';
    my $cur = first { .can-install() }, $seq;
    isa-ok $cur, HasCanInstall, 'first flattens a Seq argument instead of treating it as one item';
    ok $cur.ok, 'first picked the matching element, not the whole Seq';
}

# --- plain eager Seq of values ---
is first({ $_ > 1 }, (1, 2, 3).Seq), 2, 'first flattens a plain Seq of Ints';

# --- Hash flattens to Pairs, like map/grep ---
{
    my %h = a => 1, b => 2;
    my $r = first { .value > 1 }, %h;
    isa-ok $r, Pair, 'first flattens a Hash argument to Pairs';
    is $r.value, 2, 'first over a Hash finds the matching Pair';
}

# --- Slip always flattens ---
sub make-slip() { slip(1, 2, 3) }
is first({ $_ > 1 }, make-slip()), 2, 'first flattens a Slip argument';

# --- gather-sourced (lazy pipe) Seq must be pulled incrementally, not
# forced into a single opaque item ---
is first({ $_ > 1 }, gather { take 1; take 2; take 3 }), 2,
    'first pulls a gather-sourced Seq element by element';

done-testing;
