use v6.e.PREVIEW;
use Test;

# 6.e's DYNAMIC:: sees only dynamic symbols, and its `$*x` falls back to
# GLOBAL::<$x> before PROCESS::<$x> (#18742; rakudo 2026.11).

plan 12;

sub s1 { my $*d1 = 1; my $l1 = 2; s0() }
sub s0 {
    is DYNAMIC::<$*d1>, 1, 'a caller\'s dynamic';
    isa-ok DYNAMIC::<$l1>, Failure, 'a caller\'s plain lexical is a Failure';
    is (try DYNAMIC::<$l1>.exception.^name), 'X::Symbol::NotDynamic', '... of X::Symbol::NotDynamic';
    is DYNAMIC::.keys.grep({ .substr(1, 1) ne '*' && $_ ne '$/' && $_ ne '$!' }).elems, 0,
        'DYNAMIC::.keys lists no plain lexical';
}
s1;

{
    my $non-dynamic = 42;
    sub foo is raw { CALLERS::<$non-dynamic> }
    is (try foo.exception.^name), 'X::Symbol::NotDynamic', 'CALLERS:: fails the same way';
}

PROCESS::<$PV> = 'via PROCESS';
PROCESS::<$GV> = 'shadowed';
GLOBAL::<$GV> = 'via GLOBAL';
GLOBAL::<$GOV> = 'via GLOBAL only';
sub s2 {
    is DYNAMIC::<$*PV>, 'via PROCESS', '$*x falls back to PROCESS';
    is DYNAMIC::<$*GOV>, 'via GLOBAL only', '... and to GLOBAL';
    is DYNAMIC::<$*GV>, 'via GLOBAL', 'GLOBAL comes before PROCESS';
    ok DYNAMIC::<$*GOV>:exists, 'a GLOBAL variable exists as $*x';
    ok DYNAMIC::.keys.grep('$*GOV'), '... and is listed';
    DYNAMIC::<$*PV> := 'bound';
    DYNAMIC::<$*GV> := 'bound too';
    is $GLOBAL::GV, 'bound too', 'binding through DYNAMIC:: binds GLOBAL';
}
s2;
is PROCESS::<$PV>, 'bound', 'binding through DYNAMIC:: binds PROCESS';
