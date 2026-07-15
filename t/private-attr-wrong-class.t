use v6;
use Test;

# Reading a private attribute on a concrete invocant whose class does not
# carry the attribute must throw (rakudo: P6opaque no-such-attribute), not
# yield Nil. roast/integration/weird-errors.t test 29 (old-issue-tracker #2879).

plan 6;

class A114672 {};
class B114672 is A114672 {
    has $!x = 5;
    our method foo(A114672:) { $!x }
};
class C114672 is B114672 {};

throws-like { &B114672::foo(A114672.new) }, Exception,
    message => /'no such attribute'/,
    'private attr read on an instance of a class without the attribute throws';

is &B114672::foo(B114672.new), 5, 'exact-class invocant still reads the attribute';
is &B114672::foo(C114672.new), 5, 'subclass instance reads the inherited attribute';

# Normal method dispatch is unaffected.
class WithAttr {
    has $!y = 42;
    method get-y() { $!y }
}
is WithAttr.new.get-y, 42, 'normal private attr read works';

# An attribute assigned Nil still reads as Nil (present in the cell, no throw).
class NilAttr {
    has $!z;
    method get-z() { $!z }
}
ok NilAttr.new.get-z === Any, 'declared-but-unset private attr reads as Any without throwing';

# A closure inside the method reading the attribute also throws on a
# wrong-class invocant (GetGlobal read path).
class B2 is A114672 {
    has $!w = 7;
    our method bar(A114672:) { my $c = { $!w }; $c() }
};
throws-like { &B2::bar(A114672.new) }, Exception,
    message => /'no such attribute'/,
    'closure-captured private attr read on wrong-class invocant throws';
