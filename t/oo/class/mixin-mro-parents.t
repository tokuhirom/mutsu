use Test;

# A role mixin is a distinct subtype whose immediate parent is the wrapped class.

plan 14;

class A { }
class B is A { }
role R { }

my $mixed = B.new but R;
is $mixed.^mro.map(*.^name).join(','), 'B+{R},B,A,Any,Mu',
    'a role mixin adds a type level before its wrapped class';
is $mixed.^parents.map(*.^name).join(','), 'B,A',
    'default parents include the wrapped class and its user ancestor';
is $mixed.^parents(:local).map(*.^name).join(','), 'B',
    'the wrapped class is the direct parent';
is $mixed.^parents(:all).map(*.^name).join(','), 'B,A,Any,Mu',
    'all parents include the wrapped class and root types';
is $mixed.^mro(:roles).map(*.^name).join(','), 'B+{R},R,B,A,Any,Mu',
    'role-inclusive MRO includes both the mixed role and wrapped class';
is $mixed.^parents(:tree).raku, '[B, [A, [Any, [Mu]]]]',
    'the parent tree starts at the wrapped class';

my $native = 5 but R;
is $native.^mro.map(*.^name).join(','), 'Int+{R},Int,Cool,Any,Mu',
    'a mixin over a native value keeps its native base type';
is $native.^parents.map(*.^name).join(','), 'Int',
    'native mixin parents include the native base type';
is $native.^mro(:roles).map(*.^name).join(','),
    'Int+{R},R,Int,Real,Numeric,Cool,Any,Mu',
    'native mixin role-inclusive MRO retains builtin roles';

$mixed.^set_name('Renamed');
is $mixed.^mro[0].^name, 'Renamed', 'renaming the mixin changes its MRO head';
is $mixed.^mro.gist, '((Renamed) (B) (A) (Any) (Mu))',
    'the renamed MRO head renders with its current name';
is $mixed.^parents.map(*.^name).join(','), 'B,A',
    'renaming the mixin does not rename its base class';
is R.new.^mro.map(*.^name).join(','), 'R,Any,Mu',
    'punning a role does not add a duplicate base level';

class Composed does R { }
is (Composed.new but R).^mro(:roles).map(*.^name).join(','),
    'Composed+{R},R,Composed,R,Any,Mu',
    'the mixed role and the same base-composed role occupy separate MRO levels';
