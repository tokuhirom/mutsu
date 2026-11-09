use v6;
use Test;

# A `)` inside a quoted string of a custom attribute trait's argument list does
# not end the list (Red's `has $!bad is index({ .id }, :opclass('x); DROP'))`).
plan 5;

my @seen;
multi sub trait_mod:<is>(Attribute:D $a, :$index!) { @seen.push: $index }
multi sub trait_mod:<is>(Attribute:D $a, :$tagged!) { @seen.push: $tagged }

class C {
    has $!a is index('a)b');
    has $!b is index({ .id }, :opclass('x); DROP TABLE y'));
    has $!c is tagged("q)\"r");
    has $!d is index(1, (2, 3));
}

is @seen.elems, 4, 'all four attribute traits ran';
is @seen[0], 'a)b', 'a single-quoted ) stays in the argument';
is @seen[1][1].raku, ':opclass("x); DROP TABLE y")', 'a ; and ) inside a named argument string';
is @seen[2], 'q)"r', 'an escaped quote inside a double-quoted string';
is-deeply @seen[3].list, (1, (2, 3)), 'nested parens still balance';
