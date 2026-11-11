unit module LoopParamShadowsFileLexical;

# File-scope lexicals that the routines below shadow with `for` parameters.
my $x = 'file';
my $n = 'fileN';

package P {
    my $y = 'pkg';
    our sub pkg-loop { my @r; for 1 -> $y { @r.push: $y }; @r.push: $y; @r }
}

our sub direct is export { my @r; for 1, 2 -> $x { @r.push: $x }; @r.push: $x; @r }
our sub nested is export {
    my @r;
    for 1 -> $x { for 2 -> $x { @r.push: $x }; @r.push: $x }
    @r.push: $x;
    @r
}
our sub outer-closure is export { my &g = { $x }; my @r; for 1 -> $x { @r.push: g() }; @r }
our sub with-my is export { my @r; for 1, 2 -> $n { @r.push: $n * 10 }; @r.push: $n; @r }
our sub topic is export { my @r; for 1, 2 { @r.push: $_ ~ $x }; @r }
our sub nil-param is export { my @r; for [1, Nil] -> $x { @r.push: $x.raku }; @r }
our sub in-package is export { P::pkg-loop() }
