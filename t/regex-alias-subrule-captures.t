use v6;
use Test;

plan 7;

grammar G {
    token list { <nz> [ '/' <seg> ]* }
    token seg { \w* }
    token nz { \w+ }
}

# A `$<name> = <subrule>` alias must carry the called rule's full sub-match
# tree, not just its text (URI's `$<path-abempty> = <IETF::...::path-abempty>`).
ok 'a/b/c' ~~ /^ $<pl> = <G::list> $/, 'aliased qualified subrule matches';
is ~$<pl>, 'a/b/c', 'alias captures the text';
is-deeply $<pl><seg>.map(~*).List, ('b', 'c'),
    'alias keeps the nested quantified captures';
ok $<G::list>.defined, 'the subrule own capture is kept alongside the alias';
is-deeply $<G::list><seg>.map(~*).List, ('b', 'c'),
    'the subrule own capture keeps nested captures too';

# Same via a lexical `my regex` wrapper (the URI path-scheme shape).
my regex wrap {
    [ $<pl> = <G::list> ]
}
ok 'x/y' ~~ /^ <wrap> $/, 'my-regex wrapper matches';
is-deeply $<wrap><pl><seg>.map(~*).List, ('y',),
    'nested captures survive through the wrapper alias';
