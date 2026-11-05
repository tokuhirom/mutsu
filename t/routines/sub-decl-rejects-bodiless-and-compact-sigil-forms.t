use Test;

plan 11;

# Spellings rakudo rejects at parse time (#16210). Measured on rakudo 2026.09.
sub verdict(Str $code) {
    my $r = try EVAL $code;
    $!.defined ?? $!.^name !! 'ok';
}

is verdict('sub add($a, $b);'), 'X::UnitScope::Invalid',
    'a bodiless sub with a signature is not a forward declaration';
is verdict('multi sub add($a, $b);'), 'X::UnitScope::Invalid',
    'nor is a bodiless multi';
is verdict('sub sum4($$$$) { 1 }'), 'X::Syntax::Malformed',
    'the Perl 5 prototype spelling $$$$ is malformed';
is verdict('sub f($$) { 1 }'), 'X::Syntax::Malformed', '$$ too';
is verdict('sub f($ $) { 1 }'), 'X::Syntax::Malformed',
    'anonymous parameters need a comma';

# what stays valid
is verdict('sub f($, $) { 1 }'), 'ok', 'comma-separated anonymous parameters';
is verdict('my $c = sub ($, $) { 1 }; 1'), 'ok', 'in an anonymous sub';
is verdict('sub add($a, $b) {...}'), 'ok', 'a forward declaration is spelled {...}';
is verdict('sub MAIN($a);'), 'ok', 'the unit-scoped MAIN keeps its semicolon form';

# a stub completed later
is EVAL('sub fw($x) {...}; sub fw($x) { $x * 2 }; fw(21)'), 42,
    'a {...} stub is completed by the later definition';
is EVAL('sub four($, $, $, $) { "four" }; four(1, 2, 3, 4)'), 'four',
    'four anonymous parameters';
