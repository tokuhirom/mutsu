use Test;

# `.can` on a value mixed with one candidate of a role group (a parametric
# `role R[\u]` declared beside a bare `role R`) finds the methods of the
# candidate the mixin was built from, not those of the last-declared one
# (CSS::Units, #10310).
plan 5;

role R[\u] { method units { u }; method gist { "g" } }
role R { method value(\v, \u) { v but R[u] } }

my $v = R.value(5, 'em');
is $v.units, 'em', 'the parametric candidate answers the call';
ok $v.can('units').so, 'can finds a method of the parametric candidate';
ok $v.can('gist').so, 'can finds another one';
nok $v.can('value').so, 'but not a method of the bare candidate';

my $w = 6 but R['pt'];
ok $w.can('units').so, 'also for a direct mixin';
