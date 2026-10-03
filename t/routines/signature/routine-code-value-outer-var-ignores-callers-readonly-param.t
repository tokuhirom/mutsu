use v6;
use Test;

plan 8;

# A top-level routine called through a code value (`&setter` stored in a
# variable) assigns an outer variable that belongs to the routine's declaring
# scope. A readonly (non-`is rw`) parameter of whoever calls the code value
# must not make that assignment fail just because the two share a name
# (#11070, the code-value sibling of #10389 and #11054).

my $v = 0;
sub setter($t) { $v = $t }

sub via-variable($v) { my $s = &setter; $s($v) }
via-variable('y');
is $v, 'y', 'code value stored in a variable assigns the outer variable';

sub via-ampersand($v) { &setter($v) }
via-ampersand('amp');
is $v, 'amp', '&name(...) call assigns the outer variable';

sub apply(&f, $v) { f($v) }
apply(&setter, 'passed');
is $v, 'passed', 'code value passed as an argument assigns the outer variable';

sub via-map($v) { ($v,).map(&setter).eager }
via-map('mapped');
is $v, 'mapped', 'code value used by map assigns the outer variable';

my $n = 0;
sub bump() { $n++ }
sub call-bump($n) { my $s = &bump; $s() }
call-bump(10);
is $n, 1, 'code value increments the outer variable';

# The caller's own parameter is still readonly once the call returns.
sub still-readonly($v) {
    my $s = &setter;
    $s('x');
    try { $v = 1 };
    $!.defined;
}
ok still-readonly('a'), 'caller\'s readonly parameter is restored after the call';

# Bindings that ARE readonly where the routine was declared stay readonly.
for 1 -> $w {
    sub write-alias { $w = 5 }
    my $g = &write-alias;
    throws-like { $g() }, Exception,
        'code value still refuses to assign a readonly loop alias of its declaring scope';
}

my $imm := 42;
sub write-imm { $imm = 1 }
throws-like { my $s = &write-imm; $s() }, Exception, message => /immutable/,
    'code value still refuses to assign an outer variable bound to an immutable value';
