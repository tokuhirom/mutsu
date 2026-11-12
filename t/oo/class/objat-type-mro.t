use Test;

is ObjAt.^mro.map(*.^name).join(','), 'ObjAt,Any,Mu', 'ObjAt MRO';
is ValueObjAt.^mro.map(*.^name).join(','), 'ValueObjAt,ObjAt,Any,Mu',
    'ValueObjAt inherits ObjAt';
is ObjAt.^roles.elems, 0, 'ObjAt has no direct roles';
is ValueObjAt.^roles.elems, 0, 'ValueObjAt has no direct roles';
ok ValueObjAt ~~ ObjAt, 'ValueObjAt type object matches ObjAt';
ok Int.WHICH ~~ ObjAt, 'a value identity token matches ObjAt';
nok ObjAt ~~ ValueObjAt, 'the base is not its derived type';
nok ObjAt ~~ Cool, 'ObjAt does not inherit Cool';

done-testing;
