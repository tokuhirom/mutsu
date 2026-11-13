use Test;

# Reads of a bound scalar, a captured scalar and an object-holding scalar
# keep their meaning on the local-read and store fast paths (#17337).

plan 10;

{
    my $x := 42;
    my $s;
    $s = $x for ^3;
    is $s, 42, 'a scalar bound to a value reads it';
    dies-ok { $x = 1 }, 'and stays read-only';
}

{
    my $a = 1;
    my $b := $a;
    $a = 5;
    is $b, 5, 'a scalar bound to another sees its writes';
    $b = 7;
    is $a, 7, 'and writes through it';
}

{
    my $n = 0;
    my &inc = { $n++ };
    inc() for ^4;
    is $n, 4, 'a scalar captured by a closure reads its latest value';
}

{
    my class Box { has $.v }
    my $obj := Box.new(v => 3);
    my $s = $obj;
    is $s.v, 3, 'a scalar store of an object holds it';
    my $t = $s;
    is $t.v, 3, 'and copying it copies the reference';
    ok $t === $obj, 'to the same object';
}

{
    my @a = 1, 2;
    my $s = @a;
    @a.push: 3;
    is $s.elems, 3, 'a scalar assigned an array still shares it';
    my class A is Array { }
    my $arr = A.new(1, 2);
    my $u = $arr;
    $u.push(9);
    is $arr.elems, 3, 'as does one assigned an Array-subclass object';
}
