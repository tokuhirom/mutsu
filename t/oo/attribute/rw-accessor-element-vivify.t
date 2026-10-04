use Test;

# An element store through an argument-less accessor on an instance
# (`$o.x[0] = 5`): an rw attribute holding Any autovivifies an Array/Hash
# into the attribute, and a location holding a defined non-container value
# refuses the store with X::Assignment::RO, as rakudo does (#11653).

plan 11;

class P {
    has $.x is rw;
    has $.y is rw = 3;
    has $.z;
    has %.h;
    method el() is rw { %!h<a> }
}

{
    my $p = P.new;
    $p.x[0] = 5;
    is $p.x.raku, '$[5]', 'positional store into an rw attribute holding Any vivifies an Array';
    $p.x[2] = 7;
    is $p.x.raku, '$[5, Any, 7]', 'a later store extends the vivified Array';
}

{
    my $p = P.new;
    $p.x<k> = 5;
    is $p.x.raku, '${:k(5)}', 'associative store vivifies a Hash';
}

{
    my $p = P.new;
    $p.x[1] = 'b';
    is $p.x.raku, '$[Any, "b"]', 'vivifying past index 0 pads with Any';
}

{
    my $r = P.new(h => {a => 3});
    throws-like { $r.el[0] = 9 }, X::Assignment::RO,
        'an rw method location holding an Int refuses the element store';
    is $r.h.raku, '{:a(3)}', '... and leaves the hash alone';
}

{
    my $s = P.new;
    throws-like { $s.y[0] = 9 }, X::Assignment::RO,
        'an rw attribute holding an Int refuses the element store';
    is $s.y, 3, '... and keeps its value';
}

{
    my $t = P.new;
    throws-like { $t.z[0] = 9 }, X::Assignment::RO,
        'a non-rw attribute holding Any refuses the element store';
    ok $t.z === Any, '... and stays Any';
}

{
    my $p = P.new;
    $p.x = [1, 2];
    $p.x[1] = 20;
    is $p.x.raku, '$[1, 20]', 'an rw attribute already holding an Array is stored into';
}
