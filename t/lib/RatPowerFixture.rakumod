unit class RatPowerFixture;

# A module-exported `multi infix:<**>` that supersedes the core operator for
# an `Int` base and a Rat exponent. The consuming unit must NOT constant-fold
# `64 ** ⅓` against the core `**` before this override is imported.
subset ExpRat of Rat where * == (½, ⅓, ⅔, ¼, ¾).any;

multi infix:<**>(Int:D $base, ExpRat:D $exp) is export {
    my $real = $base ** $exp.Num;
    if $real.round =~= $real { $real.round } else { $real }
}
