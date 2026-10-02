use Test;

# An INIT or CHECK in a method of a class (or role) declared inside a routine
# or block sees the method's own lexical in its static state, and what it
# stores there is what every call of the method starts from (#10645). Each
# expectation below is what rakudo prints.

plan 16;

sub lexical-class { my class K { method m { my $z; INIT $z = 5; $z } }; K.m }
is lexical-class(), 5, 'a method of a `my class` declared in a sub';
is lexical-class(), 5, 'the same on a second call of the sub';

sub package-class { class PK { method m { my $z; INIT $z = 6; $z } }; PK.m }
is package-class(), 6, 'a method of a package-scoped class declared in a sub';

sub in-block { if True { my class K { method m { my $z; INIT $z = 9; $z } }; K.m } }
is in-block(), 9, 'a class declared in a block of a sub';

my $anon = sub { my class K { method m { my $z; INIT $z = 10; $z } }; K.m };
is $anon(), 10, 'a class declared in an anonymous sub';

{
    my class K { method m { my $z; INIT $z = 11; $z } }
    is K.m, 11, 'a class declared in a bare block of the unit';
}

sub in-role {
    my role R { method m { my $z; INIT $z = 6; $z } }
    my class C does R { }
    C.m
}
is in-role(), 6, 'a method of a role declared in a sub';

sub nested-class {
    my class A {
        my class B { method m { my $z; INIT $z = 7; $z } }
        method b { B.m }
    }
    A.b
}
is nested-class(), 7, 'a class declared in a class declared in a sub';

sub multi-method {
    my class K { multi method m(Int $x) { my $z; INIT $z = 8; $z + $x } }
    K.m(1)
}
is multi-method(), 9, 'a multi method';

sub helper-in-method {
    my class K { method m { my sub helper { my $z; INIT $z = 9; $z }; helper() } }
    K.m
}
is helper-in-method(), 9, 'a routine declared in the method';

sub two-methods {
    my class K {
        method a { my $z; INIT $z = 1; $z };
        method b { my $w; INIT $w = 2; $w }
    }
    K.a + K.b
}
is two-methods(), 3, 'two methods each with an INIT';

sub with-attribute {
    my class K { has $.v = 4; method m { my $z; INIT $z = 10; $z + $!v } }
    K.new.m
}
is with-attribute(), 14, 'a method that also reads an attribute';

sub with-check { my class K { method m { my $z; CHECK $z = 11; $z } }; K.m }
is with-check(), 11, 'a CHECK in a method';

is EVAL('sub e { my class K { method m { my $z; INIT $z = 12; $z } }; K.m }; e()'), 12,
    'in an EVAL';

sub reads-outer {
    my $y = 1;
    my class K { method m { my $z; INIT $z = 5; $z } }
    K.m + $y
}
is reads-outer(), 6, 'the enclosing sub\'s own lexical is untouched';

sub class-lexical {
    my class K { my $s = 3; method m { my $z; INIT $z = $s; $z } }
    K.m
}
is-deeply class-lexical(), Any, 'a class-body lexical is in its static state';
