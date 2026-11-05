use Test;
use NativeCall;

plan 5;

# A call's argument-source names describe that call's binding only. A method
# whose body makes another two-argument call (`CArray[uint8]` calls
# `^parameterize`) must not see the caller's `["off", Nil]`: it used to bind
# the caller's `$off` as a shared cell in the env that the caller's slot never
# adopted, which tripped the ADR-0097 section 15 env/slot invariant (#16037).
class M {
    method rs($o, $len) { my $b = CArray[uint8].new; 1 }
    method rs-int($o, $len) { my $b = CArray[int32].new; 1 }
    method rs-str($o, $len) { my $b = CArray[Str].new; 1 }
    method one($o) { my $b = CArray[uint8].new; 1 }
    method load {
        my $off = 0;
        self.rs($off, 1);
        $off += 1;
        self.rs-int($off, 2);
        $off += 1;
        self.rs-str($off, 3);
        $off += 1;
        $off
    }
    method after-call { my $off = 5; self.rs($off, 1); $off }
    method one-arg { my $off = 7; self.one($off); $off }
}

is M.new.load, 3, 'the caller variable keeps counting across calls to a CArray[T] method';
is M.new.after-call, 5, 'a read right after the call sees the plain value';
is M.new.one-arg, 7, 'a one-parameter callee (always worked) still does';

my $off = 0;
M.new.rs($off, 1);
$off++;
is $off, 1, 'a top-level variable passed to such a method is unchanged by it';

# An `is rw` parameter still aliases the caller's variable.
class W {
    method bump($x is rw, $n) { $x += $n; my $b = CArray[uint8].new; $x }
}
my $v = 10;
W.new.bump($v, 5);
is $v, 15, 'an is rw parameter still writes through to the caller variable';
