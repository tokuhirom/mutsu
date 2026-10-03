# From the P5-X distribution: `enum Index <r w x e d>` makes bareword `e` the
# enum member, not Euler's number.
use Test;
plan 4;

{
    my enum Index ( <r w x e d f> );
    is e.raku, 'Index::e', 'enum member e shadows Euler constant';
    is (e).WHAT.^name, 'Index', 'e is an Index';
    is e.value, 3, 'e has its enum value';
}
{
    my enum Tri ( <pi tau> );
    is pi.raku, 'Tri::pi', 'enum member pi shadows the constant';
}
