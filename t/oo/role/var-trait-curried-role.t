use Test;

# `my @a is R[Int,Str] = ...` (and through a constant, `constant RC =
# R[Int,Str]; my @a is RC = ...`) ties the variable to the curried role's
# pun, exactly as `my @a is SomeClass` does. mutsu fell back to a plain
# Array. Reduced from the Rake distribution's t/01-basic.rakutest.

plan 6;

role R[*@types] does Positional {
    has @!values;
    method STORE(*@values, :INITIALIZE($)) { @!values = @values; self }
    method AT-POS($i) { @!values[$i] }
    method elems { @!values.elems }
}

my @c is R[Int,Str] = 42, "foo";
is @c.WHAT.^name, 'R[Int,Str]', 'is R[...] ties to the pun';
ok @c.WHAT =:= R[Int,Str].^pun, 'the same type as the pun';
is @c[1], 'foo', 'initializer went through STORE';

constant RC = R[Int,Str];
my @b is RC = 42, "bar";
is @b.WHAT.^name, 'R[Int,Str]', 'through a constant alias';
is @b[1], 'bar', 'initializer through the alias';

class C does R[Int,Str] { }
my @a is C = 1, "x";
is @a.WHAT.^name, 'C', 'a class doing the role is unchanged';
