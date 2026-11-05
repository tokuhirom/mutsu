use v6;
use Test;

plan 6;

# The public-attribute variables `$.n`, `@.a` and `%.h` are carried through finalization as
# the parser built them (ADR-15760 slice 2).

class C {
    has $.n = 3;
    has @.a = 1, 2;
    has %.h = :k(5);
    method total { $.n + @.a.elems + %.h.elems }
    method bump { @.a.push(9); %.h<z> = 1; self }
    method interpolated { "n=$.n a=@.a[]" }
}

my $c = C.new;
is $c.total, 6, 'public attribute variables read in a method';
is $c.bump.total, 8, 'and the containers they name are mutated';
is $c.n, 3, 'the scalar attribute is unchanged';
is $c.a.join(','), '1,2,9', 'the array was pushed';
is $c.h<z>, 1, 'the hash was assigned';
is C.new.interpolated, 'n=3 a=1 2', 'public attributes interpolate in a string';
