# A class declared in expression position handles `also is Parent` /
# `also is rw` like the statement form. Found via PDF::Class
# (`PDF::COS.loader = class PDF::Class::Loader { also is PDF::COS::Loader; ... }`).
use Test;

plan 4;

class Base { method base { 'base' } }

my $c = class Foo {
    also is Base;
    has $.x;
};
is $c.^name, 'Foo', 'the expression yields the class';
is $c.new.base, 'base', 'also is Base in a class expression';
ok Foo ~~ Base, 'the parent is recorded';

my $rw = class RW { also is rw; has $.y };
my $o = $rw.new(:y(1));
$o.y = 2;
is $o.y, 2, 'also is rw in a class expression';
