use v6;
use Test;

plan 8;

# A sub called directly by name writes an outer `my` variable. The variable's
# own binding decides whether that is allowed, not a same-named readonly
# parameter of whoever called the sub (#11165, ADR-11142).

my $x = 42;
sub g4 { $x = 7 }
sub c6($x) { g4(); $x }
is c6(1), 1, 'the caller keeps its own parameter';
is $x, 7, 'a direct call assigns the outer variable named like the caller\'s readonly param';

my $n = 0;
sub bump { $n++ }
sub call-bump($n) { bump(); bump() }
call-bump(5);
is $n, 2, 'a direct call increments the outer variable';

my $s = 'a';
sub app { $s ~= 'b' }
sub call-app($s) { app() }
call-app('z');
is $s, 'ab', 'a direct call applies an assignment operator to the outer variable';

# Two levels of callers, each with its own readonly `$x`.
sub c7($x) { c6($x) }
c7(3);
is $x, 7, 'callers nested two deep with the same parameter name';

# The caller's own parameter is still readonly after the call returns.
sub still-readonly($x) {
    g4();
    try { $x = 1 };
    $!.defined;
}
ok still-readonly('a'), 'caller\'s readonly parameter is restored after the call';

# Bindings that ARE readonly stay readonly.
sub outer-param($p) {
    sub inner-write { $p = 1 }
    inner-write();
}
throws-like { outer-param(5) }, Exception,
    message => 'Cannot assign to a readonly variable or a value',
    'a nested sub still refuses to assign its enclosing routine\'s readonly param';

# (An immutable `:=` outer binding is pinned by
# routine-code-value-outer-var-ignores-callers-readonly-param.t; it cannot live
# in this file next to the `constant` below until #11263 is fixed.)

constant $c = 5;
sub write-c { $c = 1 }
sub shadow-c($c) { write-c() }
throws-like { shadow-c(1) }, Exception,
    'a constant still refuses the write from a caller with a same-named param';
