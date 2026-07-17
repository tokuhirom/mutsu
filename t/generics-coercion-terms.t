use Test;

plan 18;

# Coercion type terms: `TypeName()` with no argument is the coercion type
# `TypeName(Any)`, not a zero-argument call.
is Int().raku, 'Int(Any)', 'Int() is a coercion type term';
is Str().raku, 'Str(Any)', 'Str() is a coercion type term';
is Num().raku, 'Num(Any)', 'Num() is a coercion type term';
is Int().^name, 'Int(Any)', 'Int().^name is Int(Any)';

# A type smiley combined with the coercion term.
is Int:D().raku, 'Int:D(Any)', 'Int:D() coercion type term';
is Int:U().raku, 'Int:U(Any)', 'Int:U() coercion type term';
is Str:D().raku, 'Str:D(Any)', 'Str:D() coercion type term';

# An explicit source type keeps its parameterization.
is Int(Str).raku, 'Int(Str)', 'Int(Str) coercion type term';

# A value argument still coerces the value.
is Int("42"), 42, 'Int("42") still coerces';
is Int(3.5), 3, 'Int(3.5) still coerces';

# Generic role type parameters resolve through the smiley/coercion forms.
my role R[::T] {
    method t-nominalizables { T:D, T:U, T(), T:D() }
    method bare { T }
    method smiley-d { T:D }
    method coerce { T() }
}
my class CInt does R[Int] { }
my class CStr does R[Str] { }

is CInt.bare.^name, 'Int', 'generic T resolves to Int';
is CInt.smiley-d.raku, 'Int:D', 'generic T:D resolves to Int:D';
is CInt.coerce.raku, 'Int(Any)', 'generic T() resolves to Int(Any)';
is-deeply CInt.t-nominalizables, (Int:D, Int:U, Int(), Int:D()),
    'generic nominalizables over Int';
is-deeply CStr.t-nominalizables, (Str:D, Str:U, Str(), Str:D()),
    'generic nominalizables over Str';

# A class subclassing a parameterized native Array inherits Array methods.
class MyIntArray is Array[Int] {}
my MyIntArray $a .= new;
$a.push(1, 2, 3);
is $a.List, (1, 2, 3), 'Array[Int] subclass inherits push';
ok MyIntArray.^mro.map(*.^name).grep('Array').elems > 0,
    'Array[Int] subclass has Array in its MRO';

# A wrong-typed assignment to a typed Array subclass container is rejected.
my $threw = False;
{
    my Int @typed = 1, 2, 3;
    $threw = True if (try { @typed[0] = "not an int"; 1 }) !=== 1;
}
ok $threw, 'typed Int array rejects a Str element';
