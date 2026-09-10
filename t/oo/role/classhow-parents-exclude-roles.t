use v6;
use Test;

# `.^parents` reports parent classes only — composed roles are excluded, and
# the default form also stops at Cool/Any/Mu (matching Rakudo's ClassHOW).
# The gist strings below were verified against reference `raku`.

plan 19;

# A class that only composes a role has no class parents.
role R {}
class C does R {}
is C.^parents.gist,          '()',           'C.^parents excludes composed role R';
is C.^parents(:all).gist,    '((Any) (Mu))', ':all excludes roles, keeps Any/Mu';
is C.^parents(:local).gist,  '((Any))',      ':local of a role-only class is Any';
is C.^parents(:tree).gist,   '[(Any) [(Mu)]]', 'role-only class tree is just Any/Mu';

# A role that `is` a class flattens that class into the consumer as a parent.
role A is Exception {}
class X::Ouch does A {}
is X::Ouch.^parents.gist, '((Exception))',
    'role composed from a class contributes the class as a parent, not the role';

# Multiple composed roles: still no class parents.
role S {}
class M does R does S {}
is M.^parents.gist,         '()',      'multiple roles all excluded';
is M.^parents(:local).gist, '((Any))', 'multi-role :local is Any';

# Class inheritance is preserved (roles do not disturb it).
class Base {}
class Derived is Base {}
is Derived.^parents.gist,        '((Base))', 'inherited class kept';
is Derived.^parents(:local).gist,'((Base))', ':local kept';

# Multiple inheritance.
class P1 {}
class P2 {}
class MI is P1 is P2 {}
is MI.^parents.gist,         '((P1) (P2))', 'multiple inheritance parents';
is MI.^parents(:local).gist, '((P1) (P2))', 'multiple inheritance :local';

# Role composition + class inheritance together.
role Q {}
class QC does Q is Base {}
is QC.^parents.gist, '((Base))', 'role dropped, class kept';

# The default form stops at Cool/Any/Mu for built-in types.
is Int.^parents.gist,   '()',       'Int.^parents is empty (Cool excluded)';
is Str.^parents.gist,   '()',       'Str.^parents is empty (Cool excluded)';
is Array.^parents.gist, '((List))', 'Array.^parents keeps List, drops Cool';

# :local keeps the immediate built-in parent (Cool for Int, List for Array).
is Int.^parents(:local).gist,   '((Cool))', 'Int :local is Cool';
is Array.^parents(:local).gist, '((List))', 'Array :local is List';

# :all keeps Cool/Any/Mu but still drops roles.
is Int.^parents(:all).gist, '((Cool) (Any) (Mu))', 'Int :all chain';

# A user class subclassing a built-in.
class MyInt is Int {}
is MyInt.^parents.gist, '((Int))', 'MyInt default is Int';
