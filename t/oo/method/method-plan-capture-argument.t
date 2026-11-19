use Test;

# A Capture passed as a plain positional argument keys the method-call plan
# and the multi-dispatch caches as `Capture` (#17335). Repeated calls must
# still dispatch and bind exactly as the first one did, including against
# candidates that look inside the Capture (a sub-signature), which keep the
# whole candidate set uncached.

plan 13;

class C {
    method one($a, $c) { $c.elems }
    method typed($a, Capture:D $c is copy = \()) { $c.elems }
    multi method m(Capture $c) { 'capture ' ~ $c.elems }
    multi method m(Int $i)     { 'int' }
    multi method m(Str $s)     { 'str' }
}
my $o = C.new;

for ^2 {
    is $o.one(1, \(1, 2)), 2, "a Capture argument binds a plain parameter (round $_)";
    is $o.typed(1, \(:a, :b, 3)), 1, "a Capture argument binds Capture:D (round $_)";
}
is $o.typed(1), 0, 'the Capture:D default applies when omitted';

for ^2 {
    is $o.m(\(1, 2, 3)), 'capture 3', "a multi picks its Capture candidate (round $_)";
    is $o.m(42), 'int', "the same multi still picks Int after a Capture call (round $_)";
}
is $o.m('x'), 'str', 'and Str';

class D {
    multi method s(Capture $c ($x, $y)) { "pair $x $y" }
    multi method s(Capture $c)          { 'other ' ~ $c.elems }
}
my $d = D.new;
is $d.s(\(1, 2)), 'pair 1 2', 'a sub-signature candidate sees inside the Capture';
is $d.s(\(1, 2, 3)), 'other 3', 'a Capture of another shape picks the other candidate';
is $d.s(\(5, 6)), 'pair 5 6', 'and the sub-signature candidate again';
