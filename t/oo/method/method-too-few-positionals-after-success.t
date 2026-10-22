use v6.d;
use Test;

plan 8;

# A call short of a method's required positionals is an arity error even after
# the method has been called successfully (the call path remembers the method).
class P {
    method m($x) { 1 }
    method o($x, $y?) { 2 }
    method d($x, $y = 5) { $y }
    submethod sm($x) { 3 }
}
role R { method rm($x) { 4 } }
class K does R { }

sub arity-error(&code) {
    try code();
    $!.defined ?? $!.message.starts-with('Too few positionals passed') !! False;
}

P.new.m(3);
ok arity-error({ P.new.m() }), 'm() after a successful m(3) is refused';
ok arity-error({ P.new.m() }), '... and again';
P.new.o(1);
ok arity-error({ P.new.o() }), 'a required plus an optional parameter';
is P.new.o(1), 2, '... the optional one may be omitted';
P.new.d(1, 2);
ok arity-error({ P.new.d() }), 'a required plus a defaulted parameter';
is P.new.d(1), 5, '... the defaulted one may be omitted';
P.new.sm(1);
ok arity-error({ P.new.sm() }), 'a submethod';
K.new.rm(1);
ok arity-error({ K.new.rm() }), 'a role method';
