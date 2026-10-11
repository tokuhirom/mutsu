use v6;
use Test;
use nqp;

# BOOTInt / BOOTNum / BOOTStr are the P6int / P6num / P6str REPRs, distinct
# from the high-level Int / Num / Str (ADR-11553 §3.1). Answers checked
# against Rakudo 2026.09.
plan 22;

is nqp::bootint().^name, 'BOOTInt', 'bootint type name';
is nqp::bootnum().^name, 'BOOTNum', 'bootnum type name';
is nqp::bootstr().^name, 'BOOTStr', 'bootstr type name';

my str $s = "x";
is nqp::isstr("a"), 0, 'a Str is not P6str';
is nqp::isstr($s), 0, 'a native str is not P6str';
my $bs := nqp::box_s("a", nqp::bootstr());
is nqp::isstr($bs), 1, 'a boxed BOOTStr is P6str';
is nqp::unbox_s($bs), "a", 'unbox_s reads it back';
is nqp::isstr(nqp::bootstr()), 1, 'the type object passes its REPR test';

is nqp::isint(1), 0, 'an Int is not P6int';
my $bi := nqp::box_i(42, nqp::bootint());
is nqp::isint($bi), 1, 'a boxed BOOTInt is P6int';
is nqp::unbox_i($bi), 42, 'unbox_i reads it back';
is nqp::isnum($bi), 0, 'a BOOTInt is not P6num';

is nqp::isnum(1e0), 0, 'a Num is not P6num';
my $bn := nqp::box_n(2.5e0, nqp::bootnum());
is nqp::isnum($bn), 1, 'a boxed BOOTNum is P6num';
is nqp::unbox_n($bn), 2.5e0, 'unbox_n reads it back';

my $hi := nqp::hllize($bi);
is nqp::isint($hi), 0, 'hllize gives a high-level Int';
is $hi, 42, 'hllize keeps the value';
is nqp::isstr(nqp::hllize($bs)), 0, 'hllize of a BOOTStr is a Str';
is nqp::hllize($bs), "a", 'hllize of a BOOTStr keeps the value';
is nqp::isnum(nqp::hllize($bn)), 0, 'hllize of a BOOTNum is a Num';

my $alias := $bi;
is nqp::isint($alias), 1, 'an alias answers like its source';
is nqp::isint(nqp::create(nqp::bootint())), 1, 'create of the type is P6int';
