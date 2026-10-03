use Test;

# Assigning to a type-object rw method call whose body dies must raise that
# exception, not re-call the method with only the assigned value.

plan 2;

class K {
    method in(\c, *@s) is rw { in(c, @s) }
    multi sub in(\c, @s where { .elems == 0 }) is rw { c }
}

my %h = g => 1;
throws-like { K.in(%h, 2, 3) = 5 }, X::Multi::NoMatch, 'dispatch failure propagates';

class X::Bad is Exception { method message { 'bad' } }
class L {
    method at(\c, $k) is rw { die X::Bad.new if $k eq 'x'; c{$k} }
}
throws-like { L.at(%h, 'x') = 5 }, X::Bad, 'exception from the body propagates';
