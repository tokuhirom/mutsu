# A sigilless parameter that receives an `@`/`%` variable reports the aggregate from `.VAR` even
# on the first call that passes the variable on to a method (#16937).
use Test;
plan 9;

class C {
    method m(\v) { v.VAR.^name }
    multi method new(Iterable \value, :$in-place) { value.VAR === value }
}

my @a = 3, 1, 2;
is C.m(@a), 'Array', 'first method call with a fresh array';
is C.m(@a), 'Array', 'second method call';
my %h = a => 1;
is C.m(%h), 'Hash', 'first method call with a fresh hash';

my @b = 3, 1, 2;
ok C.new(@b, :in-place), 'a sigilless Iterable parameter sees the array as its own container';
my @c = 3, 1, 2;
class D {
    has @!array;
    method !SET(@array) { @!array := @array; self }
    multi method new(Iterable \value, :$in-place) {
        my @values;
        if value.VAR !=== value { @values = value }
        elsif value ~~ Array { @values := $in-place ?? value !! value.clone }
        self.CREATE!SET(@values);
    }
    method go { @!array[0] = 99 }
}
D.new(@c, :in-place).go;
is-deeply @c, [99, 1, 2], 'binding the in-place array writes through to the caller';
my @d = 3, 1, 2;
D.new(@d).go;
is-deeply @d, [3, 1, 2], 'without :in-place the caller keeps its array';

# A parameterized role as the invocant resolves its own type arguments first; that must not
# swallow the names of the caller's argument variables.
role P[::T] { method m(\v) { v.VAR.^name } }
my @e = 3, 1, 2;
is P[Int].m(@e), 'Array', 'a method called on a parameterized role sees the array';
is P[Int].m(@e), 'Array', 'and again';
my %i = a => 1;
is P[Int].m(%i), 'Hash', 'a hash argument';
