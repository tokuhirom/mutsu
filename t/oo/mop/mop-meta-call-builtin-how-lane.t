use Test;

plan 13;

# `Type.^meta(...)` is `Type.HOW.meta(Type, ...)`. For a builtin metaclass it is
# answered by the classhow dispatcher directly; every way to put something in
# front of the metamethod must still be honoured.

my class A {
    has $.x;
    method m { 1 }
    method !p { 2 }
}
my $a = A.new(:x(5));

# Builtin metamethods on a type object and on an instance.
is A.^find_method('m').name, 'm', '.^find_method on a type object';
is $a.^find_method('m').name, 'm', '.^find_method on an instance';
ok A.^find_method('nope').defined.not, '.^find_method of a missing method';
is A.^find_private_method('p').name, 'p', '.^find_private_method';
ok A.^find_private_method('nope').defined.not, '.^find_private_method of a missing method';
is-deeply A.^method_table.keys.sort.List, ('m', 'x'), '.^method_table';
is A.^name, 'A', '.^name';
is A.^attributes.map(*.name).List, ('$!x',), '.^attributes';

# A method wrapped afterwards is found by the lookup (hide-methods style).
my $found = A.^find_method('m');
is $found($a), 1, 'the found method runs';

# A user-declared metamethod of a name the builtin metaclass does not have.
my class C {
    method ^kind(Mu $type) { 'custom-kind' }
}
is C.^kind, 'custom-kind', 'user method ^kind';

# A user-declared metamethod that shadows a builtin one wins.
my class B {
    method ^can(Mu $type, Str $name) { 'user-can' }
}
is B.^can('anything'), 'user-can', 'user method ^can wins over the builtin';

# Calling through HOW explicitly is the same as the .^ form.
is A.HOW.find_method(A, 'm').name, A.^find_method('m').name, '.HOW.meta(T, ...) equals .^meta(...)';

# An introspection call does not disturb a later construction.
is A.new(:x(7)).x, 7, 'construction after introspection';
