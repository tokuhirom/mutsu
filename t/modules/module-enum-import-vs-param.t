use v6;
use Test;
use lib 't/lib';
use EnumParamUser;

# A unit module that imports an enum value named `literal` still reads its own
# `$literal` parameter: the enum key lives in its own namespace, so the module's
# routines cannot answer a scalar read with it (#14255). The bareword `literal`
# keeps meaning the enum value.

plan 9;

is-deeply EnumParamUser::mapped(<x y>), (:literal<x>, :literal<y>),
    'a map callback reads its $literal parameter, not the imported enum value';
is-deeply EnumParamUser::mapped-plain(<x y>), ('x', 'y'),
    'the parameter is the argument in the callback result';
is-deeply EnumParamUser::mapped-key(<x y>), ('x!', 'y!'),
    'the same holds for the enum value named dict';
is EnumParamUser::direct('q'), 'q', 'a directly called pointy block reads its parameter';
is-deeply EnumParamUser::looped(<x y>), ['x', 'y'],
    'a for loop parameter named like an enum value reads the item';
is EnumParamUser::as-term().gist, 'literal', 'the bareword is still the enum value';
is EnumParamUser::as-term().^name, 'Kinds', 'the bareword has the enum type';
is-deeply EnumParamUser::term-in-map(("x",)).map(*.^name).List, ('Kinds',),
    'the bareword in a map block is still the enum value';
is-deeply EnumParamUser::scalar-and-term().map(*.gist).List, ('mine', 'literal'),
    'a my $literal and the enum value coexist';


done-testing;
