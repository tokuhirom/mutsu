use Test;
plan 5;

# squish - remove consecutive duplicates
my @a = 1, 1, 2, 2, 3, 1, 1;
my @s = @a.squish;
is @s.join(","), "1,2,3,1", 'squish removes consecutive duplicates';

my @b = 1, 2, 3;
my @s2 = @b.squish;
is @s2.join(","), "1,2,3", 'squish on unique list is identity';

# minmax
my @c = 3, 1, 4, 1, 5, 9;
my $mm = @c.minmax;
is $mm, 1..9, 'minmax returns range from min to max';

# parse-base
is "ff".parse-base(16), 255, 'parse-base 16 works';
is "101".parse-base(2), 5, 'parse-base 2 works';
