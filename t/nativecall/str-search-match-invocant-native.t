use Test;

# Pin for the ledger §D(b) slice that extends the native string-search fast paths
# (contains / starts-with / ends-with / substr-eq, including the :i named forms) to a
# Match receiver. A (successful) Match coerces to its matched substring via .Str, so
# the same native search applies; the receiver gate was widened from Str-only to
# Str-or-Match. The interpreter still owns failed matches (Any/Nil) and :m/ignoremark.

plan 18;

my $m = "FooBar123".match(/<:alpha>+/);   # matches "FooBar"
is ~$m, 'FooBar', 'sanity: Match stringifies to matched substring';

# contains
is $m.contains("ooB"), True, 'Match.contains present';
is $m.contains("xyz"), False, 'Match.contains absent';
is $m.contains("oob", :i), True, 'Match.contains :i present';
is $m.contains("oob"), False, 'Match.contains case-sensitive';
is $m.contains("bar", 3, :i), True, 'Match.contains position + :i';
is $m.contains("123"), False, 'Match.contains excludes unmatched tail';

# starts-with / ends-with
is $m.starts-with("Foo"), True, 'Match.starts-with present';
is $m.starts-with("foo", :i), True, 'Match.starts-with :i';
is $m.starts-with("foo"), False, 'Match.starts-with case-sensitive';
is $m.ends-with("Bar"), True, 'Match.ends-with present';
is $m.ends-with("BAR", :i), True, 'Match.ends-with :i';
is $m.ends-with("123"), False, 'Match.ends-with excludes unmatched tail';

# substr-eq
is $m.substr-eq("Bar", 3), True, 'Match.substr-eq position';
is $m.substr-eq("BAR", 3, :i), True, 'Match.substr-eq position + :i';
is $m.substr-eq("Bar", 0), False, 'Match.substr-eq position mismatch';

# unicode case-insensitive over a Match
my $u = "café".match(/\w+/);
is $u.contains("FÉ", :i), True, 'Match.contains :i unicode';
is $u.starts-with("CA", :i), True, 'Match.starts-with :i unicode';
