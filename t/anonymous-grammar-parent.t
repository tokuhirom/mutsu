use Test;

plan 6;

my $g = my grammar { token TOP { \d+ } };

ok $g ~~ Grammar, 'an anonymous grammar is a Grammar';
is $g.HOW.^name, 'Perl6::Metamodel::GrammarHOW',
    'an anonymous grammar uses GrammarHOW';
is-deeply $g.^parents.map(*.^name).List,
    ('Grammar', 'Match', 'Capture'),
    'an anonymous grammar reports its inherited parents';
is-deeply $g.^mro.map(*.^name).List,
    ($g.^name, 'Grammar', 'Match', 'Capture', 'Cool', 'Any', 'Mu'),
    'an anonymous grammar MRO reaches the full Grammar chain';
is $g.parse('123').^name, $g.^name,
    'an anonymous grammar still parses into its own cursor type';

grammar NamedGrammarHOW { token TOP { \d+ } }
is NamedGrammarHOW.HOW.^name, 'Perl6::Metamodel::GrammarHOW',
    'a named grammar uses GrammarHOW too';
