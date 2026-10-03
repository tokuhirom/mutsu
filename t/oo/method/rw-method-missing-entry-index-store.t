use v6;
use Test;

# GH #11355: an argument-less `is rw` method returning a missing hash entry
# hands back a location that holds nothing yet; a subscript store or
# increment through it vivifies the container the subscript addresses into
# that entry, as rakudo does.

plan 8;

class D {
    has %.h;
    method el() is rw { %!h<a> }
}

{
    my $d = D.new;
    $d.el[0]++;
    is $d.h.raku, '{:a($[1])}', '$obj.meth[0]++ vivifies an Array';
    $d.el()[0] = 5;
    is $d.h.raku, '{:a($[5])}', '$obj.meth()[0] = v stores into it';
}

{
    my $d = D.new;
    $d.el[1] = 7;
    is $d.h.raku, '{:a($[Any, 7])}', 'a store past the end pads with Any';
}

{
    my $d = D.new;
    $d.el<k> = 3;
    is $d.h.raku, '{:a(${:k(3)})}', 'an associative store vivifies a Hash';
}

{
    my $d = D.new;
    $d.el<k>++ for ^2;
    is $d.h.raku, '{:a(${:k(2)})}', 'an associative increment, repeated';
}

{
    my $d = D.new;
    $d.el[0]++ for ^3;
    is $d.h.raku, '{:a($[3])}', 'a positional increment, repeated';
}

{
    my $d = D.new(h => { a => [1, 2] });
    $d.el[0] = 9;
    is $d.h.raku, '{:a($[9, 2])}', 'an existing Array is stored into in place';
}

{
    my $d = D.new;
    my $e = D.new;
    $d.el[0] = 1;
    is $e.h.raku, '{}', 'another instance is untouched';
}
