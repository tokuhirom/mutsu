use Test;

# `.polymod` on a mixed-in number (`$n but Role`) divides the number it wraps.
# With an infinite divisor list (`2 xx *`) it used to return an empty list,
# because the mixin read as 0 (Bitcoin's secp256k1 scalar multiplication).

plan 5;

role R { }

my $big = 0x3aba4162c7251c891207b747840551a71939b0de081f85c4e44cf7c13e41daa6;
my $mixed = $big but R;

is-deeply $mixed.polymod(2 xx *), $big.polymod(2 xx *), 'infinite divisors on a big mixed-in Int';
is $mixed.polymod(2 xx *).elems, 254, 'one digit per bit';
is-deeply (13 but R).polymod(2 xx *), (1, 0, 1, 1), 'a small mixed-in Int';
is-deeply (100 but R).polymod(10, 10), (0, 0, 1), 'finite divisors';
is-deeply ($big but R).polymod(256 xx 31), $big.polymod(256 xx 31), 'a finite repeated divisor list';
