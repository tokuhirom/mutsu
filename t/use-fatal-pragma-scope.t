use Test;

plan 10;

# use fatal inside a sub body must not leak to the caller after the sub returns.
# All fail() calls are inside subs (never mainline) because fail in mainline always throws.

sub sub_with_fatal() {
    use fatal;
    my $x = 1;  # no failure here, just activates the pragma
}

# Test 1: use fatal inside callee sub does not leak into caller sub
sub caller_no_fatal_1() {
    sub_with_fatal();
    my $f = fail "should not throw";
    return $f;
}
my $r1 = caller_no_fatal_1();
ok !$r1.defined, "use fatal in callee sub does not leak to caller (fail stays soft)";

# Test 2: closure with use fatal also does not leak to the enclosing sub
sub closure_with_fatal() {
    my $c = { use fatal; 1 };
    $c();
}
sub caller_after_closure() {
    closure_with_fatal();
    my $f = fail "should not throw 2";
    return $f;
}
my $r2 = caller_after_closure();
ok !$r2.defined, "use fatal in closure does not leak to caller sub";

# Test 3: use fatal DOES make fail() throw inside that sub's own body
sub sub_that_fails() {
    use fatal;
    fail "inner failure";
}
dies-ok { sub_that_fails() }, "use fatal inside sub causes fail() to throw inside sub";

# Test 4: when CALLER has use fatal and callee returns a Failure, caller's
# assignment of that Failure throws (fatal is active in the caller scope)
sub inner_that_fails_silently() {
    my $x = fail "inner";
    return $x;
}
sub caller_with_fatal() {
    use fatal;
    inner_that_fails_silently();  # Failure propagates, caller fatal mode catches it
}
dies-ok { caller_with_fatal() }, "Failure returned from callee throws in fatal caller";

# Test 5: outer fatal scope is still active after a non-fatal nested sub returns
sub nested_non_fatal() { my $x = fail "non-fatal"; return $x; }
sub outer_fatal_check() {
    use fatal;
    my $inner_result = nested_non_fatal();  # callee runs fine, returns Failure
    my $after = 42;
    return $after;
}
my $threw5 = False;
try { outer_fatal_check(); CATCH { default { $threw5 = True; } } }
ok $threw5, "outer fatal scope catches Failure assigned from callee's return value";

# Test 6: use strict inside a sub must not leak to the caller
sub sub_with_strict() {
    use strict;
    my $x = 1;
}
sub caller_after_strict() {
    sub_with_strict();
    return 42;
}
is caller_after_strict(), 42, "use strict inside sub does not crash caller";

# Test 7: use MONKEY-TYPING inside a sub must not leak
sub sub_with_monkey() {
    use MONKEY-TYPING;
    my $x = 1;
}
sub caller_after_monkey() {
    sub_with_monkey();
    return 99;
}
is caller_after_monkey(), 99, "use MONKEY-TYPING inside sub does not crash caller";

# Test 8: Repeated calls to sub_with_fatal do not accumulate fatal state
sub caller_repeat() {
    sub_with_fatal(); sub_with_fatal(); sub_with_fatal();
    my $f = fail "still not throwing";
    return $f;
}
my $r8 = caller_repeat();
ok !$r8.defined, "Repeated sub_with_fatal calls do not accumulate fatal state";

# Test 9: bare closure { use fatal; ... } called at mainline does not leak to caller
my $closure_at_mainline = { use fatal; 1 };
$closure_at_mainline();
my $f9 = "bar"[5];
ok !$f9.defined, "closure with use fatal called at mainline does not leak";

# Test 10: do {} block with use fatal at mainline does not leak
my $x10 = do { use fatal; 1 };
my $f10 = "bar"[5];
ok !$f10.defined, "do block with use fatal at mainline does not leak";
