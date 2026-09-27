use Test;

plan 2;

class C {
    method value() { 42 }
}

ok C.HOW.WHAT =:= Metamodel::ClassHOW,
    'the internal ClassHOW type is identical to the public metamodel type';

multi sub trait_mod:<is>(Method:D $method, :$guarded!) is export {
    die 'not a class HOW' unless $method.package.HOW.WHAT =:= Metamodel::ClassHOW;
    $method.package.^add_method: 'marker', method { 'installed' };
}

class Guarded {
    method value() is guarded { 42 }
}

is Guarded.new.marker, 'installed',
    'a HOW identity guard does not reject a normal class';
