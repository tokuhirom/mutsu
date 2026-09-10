use v6;
use Test;

plan 5;

# A hyphenated bareword that is not a declared sub is speculatively parsed as
# a forward-referenced listop call, gobbling a following block as its
# argument. A declared enum *value* is a complete nullary term, so the block
# must be left to the enclosing construct (Cro::HTTP2::GeneralParser's
# `if ... || %streams{...}.state !~~ header-c { ... }`).
enum State <header-init header-c data>;

my $x = 5;
if $x !~~ header-c {
    pass 'if-cond ending in !~~ kebab-enum-value keeps its block';
}
else {
    flunk 'if-cond ending in !~~ kebab-enum-value keeps its block';
}

if $x ~~ header-c {
    flunk 'smartmatch against kebab enum value in if-cond';
}
else {
    pass 'smartmatch against kebab enum value in if-cond';
}

if header-c {
    pass 'bare kebab enum value as whole if-cond';
}
else {
    flunk 'bare kebab enum value as whole if-cond';
}

my $hits = 0;
while $x !~~ header-c {
    $hits++;
    last;
}
is $hits, 1, 'while-cond ending in kebab enum value runs its block';

# Multi-line condition with leading || continuation, the exact Cro shape.
class Stream { has State $.state is rw }
my %streams = 5 => Stream.new(state => header-init);
if $x > 3
|| %streams{$x}.state !~~ header-c {
    pass 'multi-line || continuation with kebab enum smartmatch';
}
else {
    flunk 'multi-line || continuation with kebab enum smartmatch';
}
