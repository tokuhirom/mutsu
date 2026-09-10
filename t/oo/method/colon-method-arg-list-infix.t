use Test;

plan 8;

# A colon-method argument list is a list-prefix, so the list-infix operators
# (Z/X/meta-ops) — which are LOOSER than the comma separating the arguments —
# own the whole comma level. `blob32.new: $H Z+ $M` must be
# `blob32.new(($H Z+ $M))`, not `(blob32.new: $H) Z+ $M`.
#
# NOTE: every call below is parenthesized because a colon-arg list swallows the
# rest of the statement — including `is`'s expected value and description.

class C { method new(*@a) { "GOT[" ~ @a.join(",") ~ "]" } }

my @x = 1, 2, 3;
my @y = 10, 20, 30;

is C.new(|(@x Z+ @y)), 'GOT[11,22,33]', 'baseline: parenthesized Z+ argument';
is (C.new: @x Z+ @y), 'GOT[11,22,33]', 'colon-arg absorbs a Z meta-op';
is (C.new: @x X+ @y), 'GOT[11,21,31,12,22,32,13,23,33]', 'colon-arg absorbs an X meta-op';
is (C.new: 1, 2), 'GOT[1,2]', 'an ordinary comma list still passes two arguments';

# The Blob case from Digest::SHA1's sha1-block.
sub add-words(blob32 $H, blob32 $M --> blob32) { blob32.new: $H Z+ $M }
my $sum = add-words(blob32.new(1, 2, 3, 4), blob32.new(10, 20, 30, 40));
isa-ok $sum, Blob, 'blob32.new: $H Z+ $M builds a Blob, not a Seq';
is $sum.list, (11, 22, 33, 44), 'and holds the elementwise sums';

# `.=method:` takes the same list level.
class D { method new(*@a) { @a.join("|") } }
my $d = D;
$d .= new: @x Z+ @y;
is $d, '11|22|33', '.=new: absorbs a Z meta-op';

# The loose word-logical operators are still looser than the argument list.
is ((C.new: 'a') and (C.new: 'b')), 'GOT[b]', '`and` still terminates the colon-arg list';
