use Test;

plan 8;

my $backing = 41;

sub bind-two-frames-up($name) {
    CALLER::CALLER::.BIND-KEY($name, Proxy.new(
        FETCH => -> $ { $backing },
        STORE => -> $, $value { $backing = $value },
    ));
}

sub install-container($name) {
    bind-two-frames-up($name);
}

my $scalar = 1;
install-container('$scalar');
is $scalar, 41, 'CALLER stash BIND-KEY replaces the lexical container';
$scalar = 73;
is $backing, 73, 'assignment uses the bound Proxy STORE';
$backing = 99;
is $scalar, 99, 'reads continue to use the bound Proxy FETCH';

package StashBindKeyPackage {
    our $existing = 1;
}

StashBindKeyPackage::.BIND-KEY('$added', 42);
is StashBindKeyPackage::<$added>, 42, 'package stash binding is immediately readable';
is StashBindKeyPackage::.AT-KEY('$added'), 42, 'the materialized stash is updated';
ok StashBindKeyPackage::.EXISTS-KEY('$added'), 'the package stash reports the key';

my $source = 7;
StashBindKeyPackage::.BIND-KEY('$copied', $source);
$source = 8;
is StashBindKeyPackage::<$copied>, 8, 'a scalar argument shares its source container';
is StashBindKeyPackage::<$existing>, 1, 'existing package members remain intact';
