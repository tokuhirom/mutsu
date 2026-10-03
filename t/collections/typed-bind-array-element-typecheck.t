use Test;

# `my T $x := @a[$i]` type-checks the bound element's value; the accept is
# taken early for a plain value (#9494), every rejection stays as it was.

plan 9;

my @ch = "a", 2, Nil, "d";
my Str $a := @ch[0];
is $a, 'a', 'a Str element binds to a Str variable';
throws-like { my Str $b := @ch[1] }, X::TypeCheck::Binding, 'an Int element is rejected';
throws-like { my Str $c := @ch[2] }, X::TypeCheck::Binding, 'a Nil element is rejected';
throws-like { my Int $e := @ch[0] }, X::TypeCheck::Binding, 'a Str element for Int';

my Str $d := @ch[3];
@ch[3] = 'z';
is $d, 'z', 'the binding aliases the element';

my Cool $f := @ch[1];
is $f, 2, 'a supertype constraint';
my Str:D $g := @ch[0];
is $g, 'a', 'a definite constraint';

class P { }
my @objs = P.new;
my P $p := @objs[0];
isa-ok $p, P, 'an object element';

my @seen;
for ^3 -> $i {
    my @row = <x y z>;
    loop (my Int $j = 0; $j < @row.elems; $j = $j + 1) {
        my Str $chunk := @row[$j];
        @seen.push: $chunk;
    }
}
is @seen.join, 'xyzxyzxyz', 'rebinding in a loop';
