use v6;
use Test;

plan 8;

# Pin the for-loop topic writeback semantics for object elements
# (loop_var_unchanged Instance/Package/Enum/Rat arms): a read-only or
# attribute-mutating loop must not need the O(n) source rebuild, while a
# topic rebind must still write back to the source array.

class P { has $.x is rw }

# Attribute mutation through the loop param propagates via the shared cell
my @a = P.new(x => 1), P.new(x => 2);
for @a -> $p { $p.x = $p.x * 10 }
is @a.map(*.x).join(","), "10,20", "attribute mutation through loop param reaches source elements";

# Attribute mutation through the topic
my @t = P.new(x => 3), P.new(x => 4);
for @t { .x += 1 }
is @t.map(*.x).join(","), "4,5", "attribute mutation through topic reaches source elements";

# Topic rebind writes back to the source array
my @b = P.new(x => 1), P.new(x => 2);
for @b { $_ = P.new(x => 99) }
is @b.map(*.x).join(","), "99,99", "topic rebind writes back to source array";

# Read-only loop leaves the array intact
my @c = P.new(x => 5), P.new(x => 6);
my $sum = 0;
for @c { $sum += .x }
is $sum, 11, "read-only loop over instances computes correctly";
is @c.map(*.x).join(","), "5,6", "read-only loop leaves source elements intact";

# Type-object elements survive iteration
my @types = Int, Str;
for @types { ; }
is @types.raku, "[Int, Str]", "type-object elements survive read-only iteration";

# Rat elements: rebind still writes back
my @r = 1/2, 3/4;
for @r { $_ = $_ + 1 }
is @r.join(","), "1.5,1.75", "rat topic rebind writes back";

# given topic: attribute mutation propagates
my $p = P.new(x => 7);
given $p { .x = 8 }
is $p.x, 8, "given topic attribute mutation propagates";

done-testing;
