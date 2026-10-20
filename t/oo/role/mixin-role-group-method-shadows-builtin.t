use Test;

# A value mixed with a parametric role answers the role's own `gist`/`Str`
# over the built-in ones, also when the role has a plain candidate too
# (CSS::Units' `method gist {self ~ units}`, #14175).

plan 6;

role H[\u] { method gist { self ~ u }; method Str { 'S' ~ u } }
role H { method other { 1 } }

my $x = 1 but H['pt'];
is $x.gist, 'Sptpt', 'gist of the parametric candidate';
is $x.Str, 'Spt', 'Str of the parametric candidate';
is "$x", 'Spt', 'interpolation uses Str';
is $x.other, 1, 'the plain candidate is still composed';

role G[\d, \u] { method gist { self ~ u }; method type { u } }
role G { method plain { 'plain' } }
my $y = 2 but G['L', 'mm'];
is $y.type, 'mm', 'another method of the parametric candidate';
is $y.gist, '2mm', 'gist with two role parameters';
