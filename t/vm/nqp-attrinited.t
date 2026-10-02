use Test;
use nqp;

# `nqp::attrinited` (ADR-0121 D4): an attribute is initialized once a store
# or a read reached it -- a named argument, an initializer, BUILD/TWEAK, a
# bindattr, a method or accessor read. An attribute that only holds the seed
# construction gave it (no initializer, no argument, the type object of a
# typed scalar, an empty `@`/`%`, every slot of an `nqp::create`d object)
# is not. Each expectation matches rakudo.

plan 35;

class C { has $.x; has $.y = 5; has @.z; has $!p }

{
    my $c := C.new(x => 1);
    is nqp::attrinited($c, C, '$!x'), 1, 'a named argument initializes';
    is nqp::attrinited($c, C, '$!y'), 1, 'an initializer initializes';
    is nqp::attrinited($c, C, '@!z'), 0, 'an unpassed @ attribute is not initialized';
    is nqp::attrinited($c, C, '$!p'), 0, 'an unpassed private attribute is not initialized';
}

class D {
    has $.x; has $!y; has @.l;
    method chk { nqp::attrinited(self, D, '$!y') }
}

{
    my $d := D.new;
    is nqp::attrinited($d, D, '$!x'), 0, 'no argument: $ not initialized';
    is nqp::attrinited($d, D, '@!l'), 0, 'no argument: @ not initialized';
    is $d.chk, 0, 'attrinited on self inside a method';
    my $d2 := D.new(x => 3, l => [1]);
    is nqp::attrinited($d2, D, '$!x'), 1, 'passed $ attribute';
    is nqp::attrinited($d2, D, '@!l'), 1, 'passed @ attribute';
}

class E {
    has $.a; has $.b; has %.h;
    submethod BUILD(:$!a) { }
    submethod TWEAK { $!b = 2 }
}

{
    my $e := E.new(a => 1);
    is nqp::attrinited($e, E, '$!a'), 1, 'BUILD attributive parameter';
    is nqp::attrinited($e, E, '$!b'), 1, 'TWEAK assignment';
    is nqp::attrinited($e, E, '%!h'), 0, 'untouched % attribute with a BUILD';
    is nqp::attrinited(E.new, E, '$!a'), 1,
        'an unpassed attributive BUILD parameter still binds the attribute';
}

{
    my $f := C.new;
    is nqp::attrinited($f, C, '$!x'), 0, 'before bindattr';
    nqp::bindattr($f, C, '$!x', 7);
    is nqp::attrinited($f, C, '$!x'), 1, 'after bindattr';
    is $f.x, 7, 'the bound value reads back';
    $f.z.push(1);
    is nqp::attrinited($f, C, '@!z'), 1, 'pushing through the accessor';
}

class H {
    has $.x is rw; has $!q; has @.l;
    method readq { $!q }
    method setq { $!q = 1 }
    method rl { @!l.elems }
}

{
    my $h := H.new;
    $h.x = 4;
    is nqp::attrinited($h, H, '$!x'), 1, 'assignment through an rw accessor';
    is nqp::attrinited($h, H, '$!q'), 0, 'before any access';
    $h.readq;
    is nqp::attrinited($h, H, '$!q'), 1, 'a read in a method vivifies';
    my $h2 := H.new;
    $h2.setq;
    is nqp::attrinited($h2, H, '$!q'), 1, 'an assignment in a method';
    my $h3 := H.new;
    $h3.rl;
    is nqp::attrinited($h3, H, '@!l'), 1, 'a method reading @!l vivifies it';
    my $h4 := H.new;
    my $v = $h4.x;
    is nqp::attrinited($h4, H, '$!x'), 1, 'an accessor read vivifies';
    my $h5 := H.new;
    my $w = nqp::getattr($h5, H, '$!x');
    is nqp::attrinited($h5, H, '$!x'), 1, 'nqp::getattr vivifies';
    my $h6 := H.new;
    is nqp::attrinited($h6, H, '$!x') + nqp::attrinited($h6, H, '$!x'), 0,
        'attrinited itself does not vivify';
}

{
    my $i := nqp::create(C);
    is nqp::attrinited($i, C, '$!y'), 0, 'nqp::create runs no initializer';
    is nqp::attrinited($i, C, '@!z'), 0, 'nqp::create: @ attribute';
    nqp::bindattr($i, C, '$!y', 9);
    is nqp::attrinited($i, C, '$!y'), 1, 'nqp::create then bindattr';
}

class P { has $.x }
class Q is P { has $.y }

{
    my $q := Q.new(x => 1);
    is nqp::attrinited($q, P, '$!x'), 1, 'an inherited attribute set by its argument';
    is nqp::attrinited($q, Q, '$!y'), 0, 'the subclass attribute left alone';
}

class R {
    has $.x = Nil;
    has Int $.t;
    has $.u is built(False) = 3;
    has int $.n;
    has Int() $.c;
}

{
    my $r := R.new;
    is nqp::attrinited($r, R, '$!x'), 1, 'an explicit `= Nil` initializer counts';
    is nqp::attrinited($r, R, '$!t'), 0, 'a typed scalar holding its type object';
    is nqp::attrinited($r, R, '$!u'), 1, 'an initializer of an unbuilt attribute';
    is nqp::attrinited($r, R, '$!n'), 0, 'a native scalar holding its zero';
    is nqp::attrinited($r, R, '$!c'), 0, 'a coercion-typed scalar with no value';
}
