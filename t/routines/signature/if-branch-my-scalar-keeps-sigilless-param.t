use v6;
use Test;

# MUGS::Core (ecosystem): a `my $x` declared inside one `if` branch must stop
# shadowing a sigilless `\x` parameter when the branch block ends, so the
# `else` branch and the code after the `if` still read the parameter.
plan 5;

sub then-else(\x, $c) {
    if $c { my $x = 1; 'then' } else { x }
}
is then-else(5, False), 5, 'else branch reads the sigilless param';

sub elsif-chain(\x, $c) {
    if $c { my $x = 1; 'then' } elsif !$c { x } else { 'never' }
}
is elsif-chain(6, False), 6, 'elsif branch reads the sigilless param';

sub after-if(\x, $c) {
    if $c { my $x = 1; }
    x
}
is after-if(7, False), 7, 'code after the if reads the sigilless param';

my &blk = -> \path, $c {
    if $c { my $path = path ~ '/x'; $path } else { path }
}
is blk(8, False), 8, 'pointy block parameter';
is blk(8, True), '8/x', 'the then branch still sees its own scalar';
