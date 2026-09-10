use v6;
use Test;

# `unit class Foo is export;` — `is export` (and other lowercase traits) on a
# unit-scoped class must be parsed as a trait, NOT as a superclass named
# `export`. Regression: mutsu mis-recorded `export` as a parent and died with
# "cannot inherit from 'export' because it is unknown".
#
# EVAL of a `unit class ...;` compilation unit returns the class type object,
# so we instantiate it after the fact to check the trait/parent handling.

plan 5;

# A lowercase `is export` trait on a unit class must not be treated as a parent.
{
    my $t = EVAL 'unit class UC0 is export; has $.x = 42;';
    is $t.new.x, 42, 'unit class is export loads and constructs';
}

# `is repr(...)` on a unit class is a trait, not a parent.
{
    my $t = EVAL q{unit class UC1 is repr('P6opaque'); has $.y = 7;};
    is $t.new.y, 7, 'unit class is repr(...) is a trait, not a parent';
}

# A genuine parent on a unit class still works (uppercase name -> superclass).
{
    class UCBase { method greet { "hi" } }
    my $t = EVAL 'unit class UCChild is UCBase;';
    is $t.new.greet, 'hi', 'unit class is Parent still inherits';
}

# A parametric parent on a unit class keeps its bracket suffix.
{
    my $t = EVAL 'unit class UCList is Array[Int];';
    my $a = $t.new;
    $a.push(1); $a.push(2);
    is $a.elems, 2, 'unit class is Parent[Type] keeps parametric suffix';
}

# `is rw` on a unit class stays a trait, not a parent.
{
    my $t = EVAL 'unit class UCrw is rw; has $.z is rw = 1;';
    my $o = $t.new;
    $o.z = 5;
    is $o.z, 5, 'unit class is rw stays a trait';
}
