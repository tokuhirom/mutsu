use Test;
plan 15;

# [+] sum
my @a1 = 1, 2, 3, 4;
is ([+] @a1), 10, '[+] sums a list';

# [*] product
my @a2 = 1, 2, 3, 4;
is ([*] @a2), 24, '[*] multiplies a list';

# [-] subtraction
is ([-] 10, 3, 2), 5, '[-] left fold subtraction';

# [~] string concat
is ([~] "a", "b", "c"), "abc", '[~] concatenates strings';

# [min] / [max]
my @a3 = 5, 3, 8, 1, 4;
is ([min] @a3), 1, '[min] finds minimum';
is ([max] @a3), 8, '[max] finds maximum';

# [&&] logical and
is ([&&] True, True, True), True, '[&&] all true';
is ([&&] True, False, True), False, '[&&] short-circuits on false';

# [||] logical or
is ([||] False, False, True), True, '[||] finds first true';

# [+] with range
is ([+] 1..5), 15, '[+] works with a range';

# [*] single element
my @a4 = 42;
is ([*] @a4), 42, '[*] single element returns itself';

# [//] defined-or
my $x = Nil;
my $y = 42;
is ([//] $x, $y), 42, '[//] returns first defined value';

# [==] chain equality
is ([==] 5, 5), True, '[==] equal values';

# [gcd] greatest common divisor
is ([gcd] 12, 18), 6, '[gcd] computes gcd';

# [lcm] least common multiple
is ([lcm] 4, 6), 12, '[lcm] computes lcm';
