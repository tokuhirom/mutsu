use Test;

# A call's return no longer copies back a type name, a package-qualified
# variable or an uppercase-named scalar (ADR-12529 §3, slice 38). Each keeps
# working through its own store: a lexical type stays the routine's, a
# package variable is read from the package store, and a scalar's write goes
# through its cell. Checked against rakudo.

plan 5;

my $F = 1;
sub bump-f() { $F++ }
bump-f();
is $F, 2, 'an uppercase-named scalar written by a sub';

sub mk() { my class Inner { method v { 42 } }; Inner.new.v }
is mk(), 42, 'a lexical class a sub declares works inside it';
nok (try { ::('Inner') }).defined, 'and does not leak to its caller';

sub body { my class K { $GLOBAL::k1++ } }
body();
body();
is $GLOBAL::k1, 2, 'a package variable bumped from a class body on every call';

sub setter { EVAL q[$Foo::v = 5] }
sub bumper { $Foo::v++ }
setter();
bumper();
bumper();
is $Foo::v, 7, 'a package variable created in one routine, bumped in another';
