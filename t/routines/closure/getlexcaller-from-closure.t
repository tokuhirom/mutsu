use Test;
use nqp;

# `nqp::getlexcaller` from a closure held in a variable (#16888, EEL 0.3's
# exported `&make`, an anonymous sub, reads the action method's `$/`): the
# caller's lexical, not the closure's own fresh `$/`.

plan 4;

my &mk = sub (Mu \x) { nqp::getlexcaller('$/') };
class A { method t($/) { mk(1) } }
is A.t(41), 41, 'a method calling a closure';
is A.^find_method('t')(A, 42), 42, 'through the method object';
sub named(Mu \x) { nqp::getlexcaller('$/') }
class B { method t($/) { named(1) } }
is B.t(43), 43, 'a named sub, as before';
my $block = -> { nqp::getlexcaller('$/') };
sub caller-of-block($/) { $block() }
is caller-of-block(44), 44, 'a pointy block';
