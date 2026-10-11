use Test;

# A `my $x` in a nested loop body shadows the enclosing `for` loop's
# parameter `$x`; after the inner loop the parameter is visible again (#10310).
plan 4;

my @r;
for (5,) -> Int $p {
    for 1..2 -> \side {
        my $p := "n$_" given side;
        @r.push: $p;
    }
    @r.push: $p;
}
is-deeply @r, ["n1", "n2", 5], 'inner `my $p :=` does not clobber the parameter';

@r = ();
for (5,) -> $q {
    for 1..2 { my $q = "x" ~ $_; }
    @r.push: $q;
}
is-deeply @r, [5], 'inner `my $q =` in a plain for body';

@r = ();
for (5,) -> $s {
    { my $s = "x"; }
    if True { my $s = "y"; }
    @r.push: $s;
}
is-deeply @r, [5], 'bare block and if body';

@r = ();
for 1..2 -> $t {
    for 1..2 { my $t = "x"; }
    @r.push: $t;
}
is-deeply @r, [1, 2], 'on every iteration of the outer loop';
