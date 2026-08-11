# Fixture for t/slang-tuxic-activation.t: a module that activates
# Slang::Tuxic for its own compilation unit only.
use Slang::Tuxic;

sub scoped-spaced-add($a, $b) is export { my $r = tuxic-mul (2, 1); $a + $b + $r - 2 }
sub tuxic-mul($a, $b) { $a * $b }
