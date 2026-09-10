use Test;

# An inline grammar action (`{ make … }` inside a rule, not a separate
# `.parse(:actions)` action class) sets that rule's `.made`. That value must be
# attached to the SUBRULE's Match node so a parent inline action reading
# `$<sub>.made` / `$<sub>».made` — and any post-parse `$m<sub>.made` — sees the
# produced value rather than `Any`. (PLAN §8.16; Time::Duration::Parser.)

plan 10;

grammar G {
    rule TOP  { <time>+ % <sep> { make [+] $<time>».made } }
    rule time { <number> { make +$<number> * 10 } }
    token number { \d+ }
    token sep { <.ws> }
}

my $r = G.parse("1 2 3");
is $r.made, 60, 'parent inline action reduces child .made values';
is $r<time>».made.join(','), '10,20,30', 'each subrule Match carries its own .made';
is $r<time>[0].made, 10, 'first subrule .made';
is $r<time>[2].made, 30, 'last subrule .made';

# Single (non-quantified) subrule: `$<sub>.made` (scalar) resolves too.
grammar H {
    rule TOP { <word> { make $<word>.made ~ '!' } }
    token word { \w+ { make $/.Str.uc } }
}
my $h = H.parse("hello");
is $h.made, 'HELLO!', 'scalar subrule .made available to parent inline action';
is $h<word>.made, 'HELLO', 'scalar subrule Match carries .made';

# Two levels of nesting: grandchild .made propagates up through the child.
grammar J {
    rule  TOP  { <pair> { make $<pair>.made } }
    rule  pair { <a> <b> { make $<a>.made + $<b>.made } }
    token a    { \d+ { make +$/ * 2 } }
    token b    { \d+ { make +$/ * 3 } }
}
my $j = J.parse("5 7");
is $j.made, 31, 'two-level nesting: (5*2) + (7*3) = 31';
is $j<pair>.made, 31, 'intermediate node carries reduced .made';
is $j<pair><a>.made, 10, 'grandchild a .made';
is $j<pair><b>.made, 21, 'grandchild b .made';
