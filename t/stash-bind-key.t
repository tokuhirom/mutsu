use Test;

plan 3;

my $backing = 41;

sub install-container($name) {
    CALLER::.BIND-KEY($name, Proxy.new(
        FETCH => -> $ { $backing },
        STORE => -> $, $value { $backing = $value },
    ));
}

my $scalar = 1;
install-container('$scalar');
is $scalar, 41, 'CALLER stash BIND-KEY replaces the lexical container';
$scalar = 73;
is $backing, 73, 'assignment uses the bound Proxy STORE';
$backing = 99;
is $scalar, 99, 'reads continue to use the bound Proxy FETCH';
