use Test;

plan 8;

# A method wrapper is invoked with the invocant prepended to the method's
# arguments. The call-site "argument source variable" names recorded by the
# call opcode cover the method's arguments only, so they must be shifted by
# one for the wrapper's signature. Without the shift a sigilless (`\SELF`) or
# `is raw`/`is rw` first parameter -- which re-reads its value from the named
# source variable rather than from the argument slot -- bound the wrapper's
# invocant to the FIRST ARGUMENT whenever that argument was a bare variable.

class C {
    method who($a, $b) { "who({$a},{$b}) on {self.^name}" }
}
C.^lookup('who').wrap: -> \SELF, |c {
    die "wrapper invocant is {SELF.^name}, expected C" unless SELF ~~ C;
    callsame
};

my $c = C.new;
my $x = 1;

is $c.who(1, 2),        'who(1,2) on C', 'literal arguments';
is $c.who($x, 2),       'who(1,2) on C', 'bare variable as first argument';
is $c.who(1, $x),       'who(1,1) on C', 'bare variable as second argument';
is $c.who($x, $x),      'who(1,1) on C', 'bare variables in every position';

# `is raw` first parameter takes the same source-name path.
class D {
    method who($a) { "who({$a}) on {self.^name}" }
}
D.^lookup('who').wrap: -> $SELF is raw, $a is raw {
    die "wrapper invocant is {$SELF.^name}, expected D" unless $SELF ~~ D;
    callsame
};
my $d = D.new;
my $y = 7;
is $d.who(7),  'who(7) on D', 'is raw wrapper, literal argument';
is $d.who($y), 'who(7) on D', 'is raw wrapper, bare variable argument';

# The shift must not break writeback through a wrapper: an `is rw` argument
# still names the caller's variable at its (shifted) position.
class E {
    method bump($n is rw) { $n++; 'bumped' }
}
E.^lookup('bump').wrap: -> \SELF, |c {
    die "wrapper invocant is {SELF.^name}, expected E" unless SELF ~~ E;
    callsame
};
my $n = 41;
is E.new.bump($n), 'bumped', 'rw argument through a wrapper returns';
is $n, 42, 'rw argument through a wrapper still writes back to the caller';
