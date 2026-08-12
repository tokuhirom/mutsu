use v6;
use Test;

# Each class-body statement compiles as its own chunk, so a read of a `my`
# hash/array declared by an EARLIER body statement was compiled
# package-qualified (%C::predef) while the declaration flushed to env under
# the bare sigiled name (%predef). Without the bare-name fallback the read
# silently produced a fresh empty Hash/Array.
# (Text::CSV's `%predef-hooks<not-empty> = %predef-hooks<not_empty>;` alias
# rows assigned Any because the RHS read an empty hash.)

plan 6;

class C {
    my %predef = a => 1, b => 2;
    %predef<c> = %predef<a>;          # RHS reads the class-body my hash
    my $keys-in-body = %predef.keys.sort.join(",");
    my @arr = 1, 2;
    my $arr-in-body = @arr.join(",");
    @arr.push(@arr[0] + @arr[1]);     # element read + push
    method keys-in-body { $keys-in-body }
    method arr-in-body  { $arr-in-body }
    method h { %predef.keys.sort.join(",") }
    method c-val { %predef<c> }
    method a { @arr.join(",") }
}

is C.keys-in-body, "a,b,c", "class-body statement reads the my hash declared earlier";
is C.h, "a,b,c", "method sees all class-body writes";
is C.c-val, 1, "element copied from a class-body hash read is the real value";
is C.arr-in-body, "1,2", "class-body statement reads the my array declared earlier";
is C.a, "1,2,3", "array element reads during class body see the real elements";

# A foreign qualifier must NOT reach another package's my lexical
class D {
    my %hidden = secret => 1;
    method peek { %hidden.elems }
}
is D.peek, 1, "sibling class my hash stays intact";
