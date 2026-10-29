use Test;

# A punned lexical role (`my role R {}; R.new`) is named `R`, as a package role
# is, not `R+{R}`. Found with the annotations dist (#15259).

plan 9;

my role TC { method tag { 'tc' } }
my $o = TC.new;
is $o.^name, 'TC', 'a punned lexical role instance is named after the role';
is $o.WHAT.raku, 'TC', '.WHAT.raku agrees';
is $o.tag, 'tc', 'its method works';
is-deeply $o.^roles.map(*.^name).List, ('TC',), '.^roles lists the role';
is-deeply $o.^mro.map(*.^name).List, ('TC', 'Any', 'Mu'), '.^mro has no mixin entry';
ok $o ~~ TC, 'it does the role';

role PC { }
is PC.new.^name, 'PC', 'a package role pun is named the same way';

class W { }
is (W.new but TC).^name, 'W+{TC}', 'a lexical role mixed onto another base keeps the suffix';

class H { has @.a is TC }
is H.new.a.^name, 'TC', 'a container-type trait naming a lexical role gives the role name';
