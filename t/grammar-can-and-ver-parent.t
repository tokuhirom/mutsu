use Test;

# Two grammar fixes:
#  1. `.can('parse')` reports the inherited Grammar parse methods.
#  2. A type adverb (`:ver<...>`) on a grammar name no longer eats the
#     following `is Parent` clause, so grammar inheritance composes.

plan 6;

grammar G { token TOP { \d+ } }
ok so G.can('parse'),     'grammar.can("parse") is true';
ok so G.can('subparse'),  'grammar.can("subparse") is true';
ok so G.can('parsefile'), 'grammar.can("parsefile") is true';

grammar Base:ver<1.0> {
    token ipv4 { \d+ '.' \d+ '.' \d+ '.' \d+ }
}
grammar Child:ver<1.0> is Base {
    token TOP  { <host> }
    token host { <ipv4> | \w+ }
}

ok Child.^mro.map(*.^name).grep('Base'), 'versioned grammar inherits its parent (MRO has Base)';

my $m = Child.parse('1.2.3.4');
ok $m.defined, 'child grammar matches via inherited subrule';
is ~$m<host>, '1.2.3.4', 'inherited subrule captured the host';
