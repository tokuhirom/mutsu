use Test;

plan 4;

sub is-item(\x) { x.VAR !=== x }
sub swap(\x, \y) {
    my $z = y;
    y = x;
    x = $z;
}
sub pass-on(&c, |args) { c(|args) }

nok is-item((1, 2, 3)), 'flattening list argument is not itemized for sigilless param';
ok is-item($[1, 2, 3]), 'itemized bracket argument stays itemized for sigilless param';

my $a = 5;
my $b = 3;
EVAL 'swap($a, $b)';
is "$a|$b", '3|5', 'sigilless aliases are writable through EVAL calls';

is pass-on({ $:l ~ $:w }, :w<6>, :l<p>), 'p6', '|args forwards named arguments';
