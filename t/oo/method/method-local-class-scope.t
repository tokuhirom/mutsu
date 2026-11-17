use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

class Local {
    method make-name {
        my class Inner { method hi { 'hi' } }
        Inner.^name
    }
}
is Local.make-name, 'Local::Inner', 'a method sees the class it declares';
is Local.make-name, 'Local::Inner', 'the method can declare it again';

class Escaper {
    method make-type {
        my class Hidden { method hi { 'hi' } }
        Hidden
    }
}
my $escaped = Escaper.make-type;
is $escaped.new.hi, 'hi', 'an escaped method-local class remains usable';

class BodyOwner {
    my class Nested { }
    method name { Nested.^name }
}
is BodyOwner.name, 'BodyOwner::Nested', 'a class-body type stays visible to methods';

throws-like {
    EVAL q[class Other {
        method m { my class Hidden { }; Hidden.^name }
        method k { Hidden.^name }
    }; Other.m; Other.k];
}, X::Undeclared::Symbols, 'a method-local class is invisible to sibling methods';

throws-like {
    EVAL q[class Failed {
        method m { my class Hidden { }; die 'stop' }
        method k { Hidden.^name }
    }; try Failed.m; Failed.k];
}, X::Undeclared::Symbols, 'a method-local class stays hidden after an exception';
