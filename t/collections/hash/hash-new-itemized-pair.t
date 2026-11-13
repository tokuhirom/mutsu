use Test;

plan 6;

# An itemized Pair is still a Pair to a hash initializer: Map.new / Hash.new
# read the Pair inside the Scalar instead of taking it as a lone key.
my $pr = "a" => 5;

is Map.new(($pr,)).raku, 'Map.new((:a(5)))', 'Map.new with a list holding an itemized Pair';
is Hash.new(($pr,)).raku, '{:a(5)}', 'Hash.new with a list holding an itemized Pair';
is Map.new($pr, "b" => 1).raku, 'Map.new((:a(5),:b(1)))', 'itemized Pair next to a plain one';

my @a = $pr, "c" => 2;
is Map.new(@a).raku, 'Map.new((:a(5),:c(2)))', 'Array of Pairs (elements are Scalars)';
is Map.new(($pr, $pr)).elems, 1, 'the same itemized Pair twice is one key';

my %h = ($pr,);
is %h.raku, '{:a(5)}', 'hash assignment agrees';
