use Test;

# From the ecosystem distribution P5sleep: `&CORE::sleep-timer` and
# `&CORE::sleep-till` must resolve like `&CORE::sleep`.
plan 5;

is &CORE::sleep-timer(0), 0, '&CORE::sleep-timer returns the unslept remainder';
is &CORE::sleep-timer.name, 'sleep-timer', '&CORE::sleep-timer is a routine';
ok &CORE::sleep-till.defined, '&CORE::sleep-till resolves';
ok &CORE::sleep.defined, '&CORE::sleep resolves';

sub mysleep(Int() $s) { ($s - &CORE::sleep-timer($s)).Int }
is mysleep(0), 0, 'the P5sleep idiom works';
