use Test;
use MONKEY-SEE-NO-EVAL;

# A default on a slurpy or capture parameter is a compile-time
# X::Parameter::Default in every kind of signature (sub, pointy block), for
# the anonymous and the named spellings alike (#16393).
my @slurpies = '*@', '**@', '+@', '|', '*%', '|c', '+a', '*@a', '*%h';
plan @slurpies * 2 + 2;

for @slurpies -> $slurpy {
    throws-like "-> $slurpy = 42 \{\}()", X::Parameter::Default,
        "pointy block: $slurpy with a default";
    throws-like "sub f($slurpy = 42) \{\}", X::Parameter::Default,
        "sub: $slurpy with a default";
}

# `'a' x Inf` is a Failure wrapping X::NYI, not an exception.
my $r = 'a' x Inf;
isa-ok $r, Failure, "'a' x Inf is a Failure";
isa-ok $r.exception, X::NYI, 'wrapping X::NYI';
$r.so;
