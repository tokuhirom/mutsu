use Test;
plan 20;

# >>op<< strict mode (same length)
my @a = 1, 2, 3;
my @b = 10, 20, 30;

my @r1 = @a >>+<< @b;
is @r1.join(" "), "11 22 33", '>>+<< element-wise addition';

my @r2 = @a >>*<< @b;
is @r2.join(" "), "10 40 90", '>>*<< element-wise multiplication';

my @r3 = @a >>-<< @b;
is @r3.join(" "), "-9 -18 -27", '>>-<< element-wise subtraction';

# concatenation
my @s1 = "a", "b", "c";
my @s2 = "1", "2", "3";
my @r4 = @s1 >>~<< @s2;
is @r4.join(" "), "a1 b2 c3", '>>~<< element-wise concat';

# exponentiation
my @bases = 2, 3, 4;
my @exps = 3, 2, 2;
my @r5 = @bases >>**<< @exps;
is @r5.join(" "), "8 9 16", '>>**<< element-wise power';

# division
my @nums = 10, 20, 30;
my @dens = 2, 4, 5;
my @r6 = @nums >>/<< @dens;
is @r6.join(" "), "5 5 6", '>>/<< element-wise division';

# >>op>> right DWIM (right cycles to match left)
my @left = 1, 2, 3, 4;
my @right = 10;
my @r7 = @left >>+>> @right;
is @r7.join(" "), "11 12 13 14", '>>+>> scalar extension on right';

my @rr = 1, 2;
my @r8 = @left >>+>> @rr;
is @r8.join(" "), "2 4 4 6", '>>+>> right cycles to match left';

# <<op<< left DWIM (left cycles to match right)
my @short = 10;
my @long = 1, 2, 3;
my @r9 = @short <<+<< @long;
is @r9.join(" "), "11 12 13", '<<+<< scalar extension on left';

my @ss = 1, 2;
my @r10 = @ss <<*<< @long;
is @r10.join(" "), "1 4 3", '<<*<< left cycles to match right';

# <<op>> both DWIM (shorter extends to match longer)
my @x = 1, 2, 3, 4, 5;
my @y = 10, 20;
my @r11 = @x <<+>> @y;
is @r11.join(" "), "11 22 13 24 15", '<<+>> shorter cycles to match longer';

my @r12 = @y <<+>> @x;
is @r12.join(" "), "11 22 13 24 15", '<<+>> commutative behavior';

# empty arrays
my @empty;
my @nonempty = 1, 2, 3;
my @r13 = @empty <<+>> @nonempty;
is @r13.join(" "), "1 2 3", '<<+>> empty left with nonempty right';

my @r14 = @nonempty <<+>> @empty;
is @r14.join(" "), "1 2 3", '<<+>> nonempty left with empty right';

# chaining hyper operators
my @c1 = 1, 2, 3;
my @c2 = 10, 20, 30;
my @c3 = 100, 200, 300;
my @r15 = @c1 >>+<< @c2 >>+<< @c3;
is @r15.join(" "), "111 222 333", 'chaining hyper operators';

# modulus
my @mods = 10, 23, 37;
my @divs = 3, 5, 7;
my @r16 = @mods >>%<< @divs;
is @r16.join(" "), "1 3 2", '>>%<< element-wise modulus';

# ASCII hyper
my @r17 = @a >>+<< @b;
is @r17.join(" "), "11 22 33", 'ASCII hyper works';

# comparison
my @cmp1 = 1, 2, 3;
my @cmp2 = 1, 2, 3;
my @r18 = @cmp1 >>==<< @cmp2;
is @r18.join(" "), "True True True", '>>==<< element-wise equality';

# result is an array
my @res = @a >>+<< @b;
is @res.elems, 3, 'hyper result has correct number of elements';
is @res[1], 22, 'hyper result element access works';
