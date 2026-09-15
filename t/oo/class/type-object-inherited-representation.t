use Test;

# A :D method inherited from a parent is not applicable to the child type
# object. It must not suppress the native type-object raku/gist methods.

plan 2;

class Parent {
    multi method raku(Parent:D:) { 'parent-instance-raku' }
    multi method gist(Parent:D:) { 'parent-instance-gist' }
}
class Child is Parent { }

is Child.raku, 'Child', 'a child type object keeps the native raku representation';
is Child.gist, '(Child)', 'a child type object keeps the native gist representation';
