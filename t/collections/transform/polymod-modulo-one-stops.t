use Test;
plan 12;

# A divisor of 1 stops the decomposition for a finite divisor list too, as it
# does for a lazy one (roast S32-num/polymod.t "modulo 1 stops"): the rest of
# the number is the last element. Expected values are rakudo 2026.09's.
is-deeply 120.polymod(10, 1),                  (0, 12),   'a trailing 1 ends the list';
is-deeply 120.polymod(10, 1, 5),               (0, 12),   'divisors after a 1 are not used';
is-deeply 120.polymod(1, 1),                   (120,),    'a leading 1 leaves the number whole';
is-deeply 120.polymod(5, 1, 1),                (0, 24),   'repeated 1s end it once';
is-deeply 1.polymod(1),                        (1,),      '1.polymod(1)';
is-deeply 0.polymod(1),                        (0,),      '0.polymod(1)';
is-deeply 120.polymod(1, 10, 100, 1000, 10000), (120,),   'the documented 1, 10, 10², ... list';
is-deeply 120.polymod(10, 100),                (0, 12, 0), 'no 1: the full decomposition';
is-deeply (7/2).polymod(1),                    (7/2,),    'a Rat with a divisor of 1';
is-deeply 5e0.polymod(1),                      (5e0,),    'a Num with a divisor of 1';
is-deeply 100.polymod(1 xx *),                 (100,),    'a lazy 1 list (unchanged)';
is-deeply 100.polymod(10, 1 xx *),             (0, 10),   'a lazy list after a divisor (unchanged)';
