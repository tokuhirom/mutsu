use Test;
plan 23;

# === R (Reverse) meta operator ===
# R reverses the arguments of an infix operator
is 5 R- 3, -2, 'R- reverses subtraction: 3 - 5 = -2';
is 2 R** 3, 9, 'R** reverses exponentiation: 3 ** 2 = 9';
is 10 R/ 2, 0.2, 'R/ reverses division: 2 / 10 = 0.2';
is 10 R% 3, 3, 'R% reverses modulo: 3 % 10 = 3';
is "world" R~ "hello ", "hello world", 'R~ reverses concatenation';

# R with comparison operators
is (1 Rcmp 2), Order::More, 'Rcmp reverses comparison: 2 cmp 1 = More';

# === X (Cross) meta operator ===
# X applies operator across all pairings (Cartesian product)
my @a = 1, 2;
my @b = 10, 20;

my @r1 = @a X+ @b;
is @r1.join(" "), "11 21 12 22", 'X+ cross addition';

my @r2 = @a X* @b;
is @r2.join(" "), "10 20 20 40", 'X* cross multiplication';

my @s1 = "a", "b";
my @s2 = "1", "2";
my @r3 = @s1 X~ @s2;
is @r3.join(" "), "a1 a2 b1 b2", 'X~ cross concatenation';

# X with single elements
my @r4 = 1, 2 X+ 10;
is @r4.join(" "), "11 12", 'X+ with scalar right';

# X with three elements each
my @x1 = 1, 2, 3;
my @x2 = 10, 20, 30;
my @r5 = @x1 X+ @x2;
is @r5.join(" "), "11 21 31 12 22 32 13 23 33", 'X+ with 3x3 elements';

# X producing comparison results
my @c1 = 1, 2;
my @c2 = 2, 1;
my @r6 = @c1 X== @c2;
is @r6.join(" "), "False True True False", 'X== cross equality';

# === Z (Zip) meta operator ===
# Z applies operator across parallel positions
my @z1 = 1, 2, 3;
my @z2 = 10, 20, 30;

my @r7 = @z1 Z+ @z2;
is @r7.join(" "), "11 22 33", 'Z+ zip addition';

my @r8 = @z1 Z* @z2;
is @r8.join(" "), "10 40 90", 'Z* zip multiplication';

my @r9 = @z1 Z- @z2;
is @r9.join(" "), "-9 -18 -27", 'Z- zip subtraction';

# Z with string ops
my @zs1 = "a", "b", "c";
my @zs2 = "1", "2", "3";
my @r10 = @zs1 Z~ @zs2;
is @r10.join(" "), "a1 b2 c3", 'Z~ zip concatenation';

# Z truncates to shorter list
my @short = 1, 2;
my @long = 10, 20, 30, 40;
my @r11 = @short Z+ @long;
is @r11.join(" "), "11 22", 'Z+ truncates to shorter list';

my @r12 = @long Z+ @short;
is @r12.join(" "), "11 22", 'Z+ truncates to shorter list (reversed)';

# Z with comparison
my @zc1 = 1, 2, 3;
my @zc2 = 1, 3, 3;
my @r13 = @zc1 Z== @zc2;
is @r13.join(" "), "True False True", 'Z== zip equality';

# Z with modulo
my @zm1 = 10, 20, 30;
my @zm2 = 3, 7, 8;
my @r14 = @zm1 Z% @zm2;
is @r14.join(" "), "1 6 6", 'Z% zip modulo';

# Z with exponentiation
my @ze1 = 2, 3, 4;
my @ze2 = 3, 2, 2;
my @r15 = @ze1 Z** @ze2;
is @r15.join(" "), "8 9 16", 'Z** zip exponentiation';

# Z with min/max
my @m1 = 5, 1, 8;
my @m2 = 3, 7, 2;
my @r16 = @m1 Zmin @m2;
is @r16.join(" "), "3 1 2", 'Zmin zip minimum';

my @r17 = @m1 Zmax @m2;
is @r17.join(" "), "5 7 8", 'Zmax zip maximum';
