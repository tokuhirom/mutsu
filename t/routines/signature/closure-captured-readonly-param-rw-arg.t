use Test;

# A closure that passes a captured readonly `$` parameter to an `is rw`
# parameter is refused like the routine's own body is (#14209). Expected
# values are the `raku` output.

plan 5;

sub g($v is rw) { $v = 'x' }

sub f($p) { my &c = { g($p) }; c(); $p }
throws-like { f(1) }, X::Parameter::RW, 'a captured readonly parameter is refused';

sub direct($p) { g($p); $p }
throws-like { direct(1) }, X::Parameter::RW, 'the routine body itself still refuses it';

sub copy($p is copy) { my &c = { g($p) }; c(); $p }
is copy(1), 'x', 'a captured is-copy parameter is writable';

sub shadow($p) { my &c = { my $p = 1; g($p); $p }; c() }
is shadow(1), 'x', 'a closure-local rebinding of the name is writable';

sub own($p) { my &c = -> $p is rw { g($p); $p }; my $q = 1; c($q) }
is own(1), 'x', 'the closure own parameter shadows the captured one';
