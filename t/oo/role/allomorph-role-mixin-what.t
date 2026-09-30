use Test;

# An allomorph with a role mixed in (`<42> but R`) is the composed type
# `IntStr+{R}`: its `.WHAT` and MRO head keep both the allomorph type and the
# role, and `.^mro[0] === .WHAT` holds (#10296, ADR-0060).

plan 14;

role R {}
my $x = <42> but R;

is $x.WHAT.^name, 'IntStr+{R}', '.WHAT keeps the role on an allomorph';
is $x.^name, 'IntStr+{R}', '.^name matches';
is $x.^mro.map(*.^name).join(' '),
    'IntStr+{R} IntStr Allomorph Str Int Cool Any Mu', '.^mro head is the composed type';
is $x.^mro(:roles).map(*.^name).join(' '),
    'IntStr+{R} R IntStr Allomorph Str Stringy Int Real Numeric Cool Any Mu',
    '.^mro(:roles) head is the composed type';
ok $x.^mro[0] === $x.WHAT, '.^mro[0] === .WHAT';
ok $x.WHAT === (<7> but R).WHAT, 'same composition shares one type object';
nok $x.WHAT === (7 but R).WHAT, 'differs from the Int+{R} composition';
ok $x.WHAT ~~ R, 'type object does the role';
ok $x.WHAT ~~ IntStr, 'type object is still an IntStr';
is <42>.WHAT.^name, 'IntStr', 'a bare allomorph stays its allomorph type';
is (<1.5> but R).^mro[0].^name, 'RatStr+{R}', 'RatStr composes the same way';
is (42 but R).WHAT.^name, 'Int+{R}', 'a plain Int mixin is unaffected';

my $z = <5> but R;
$z.^set_name('Renamed');
is $z.^name, 'Renamed', '.^set_name on the value renames the composition';
is $z.WHAT.^name, 'Renamed', '... seen through .WHAT too';
