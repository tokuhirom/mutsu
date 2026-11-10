use Test;

# The mixin class of a `Type but Role` type object is found through an index
# (formerly a scan of every registered class per lookup); it must stay correct
# as new mixin classes are registered between lookups.

plan 7;

role R { method hi { "hi" } }
role S { method yo { "yo" } }
class C { }
class D { }

my $t = C but R;
is $t.^name, "C+{R}", "first mixin type object";
is $t.hi, "hi", "its method";
my $u = D but S;
is $u.^name, "D+{S}", "a mixin registered later";
is $u.yo, "yo", "its method";
is $t.hi, "hi", "the earlier one still resolves";
my $v = C but S;
is $v.yo, "yo", "another mixin of the same base";
is (C but R).^name, $t.^name, "the same mixin is the same type";
