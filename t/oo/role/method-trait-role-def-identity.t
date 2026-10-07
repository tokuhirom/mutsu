use Test;

# ADR-11827 phase 3: a role a method trait composes onto the method belongs to
# that method declaration (not to a name), so every method object a lookup
# hands out carries it and a same-named method elsewhere does not.

plan 8;

role Tag { method tagged { True } }
multi trait_mod:<is>(Method $m, :$tag!) { $m does Tag }

class A {
    method one is tag { 1 }
    method two { 2 }
    multi method m(Int $x) is tag { 'int' }
    multi method m(Str $x) { 'str' }
}
class B is A { method two is tag { 22 } }
class C { method one { 'other' } }

is A.^find_method('one') ~~ Tag, True, '.^find_method sees the trait-composed role';
is A.^lookup('one').tagged, True, 'its methods dispatch';
is A.^find_method('two') ~~ Tag, False, 'an untagged method of the same class does not';
is B.^find_method('two') ~~ Tag, True, 'a subclass override declares its own tag';
is C.^find_method('one') ~~ Tag, False, 'a same-named method of another class does not';
is-deeply A.^methods.grep(Tag).map(*.name).sort.list, ('one',),
    '.^methods.grep(Tag) finds exactly the tagged declarations';
is A.^find_method('m').candidates.grep(Tag).elems, 1,
    'only the tagged multi candidate carries the role';
is A.new.m(3), 'int', 'tagging does not disturb dispatch';
