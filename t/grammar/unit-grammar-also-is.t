use MONKEY-SEE-NO-EVAL;
use Test;

grammar BaseG {
    token ws { \s* }
    rule TOP { ^ 'x' $ }
}

my $derived = EVAL q:to/CODE/;
    unit grammar DerivedG;
    also is BaseG;
    rule TOP { ^ 'y' $ }
    CODE

plan 3;
ok $derived.parse('y'), 'unit grammar also is parent parses its own rule';
ok $derived.parse('x') ~~ Nil,
    'unit grammar also is parent does not use the inherited grammar TOP';
ok $derived.^mro.map(*.^name).grep('BaseG'),
    'also is parent appears in the grammar MRO';
