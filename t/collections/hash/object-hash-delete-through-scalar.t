use Test;

plan 8;

# A `Hash[V, K]` held in a `$` keys its entries by the key object, so
# `$h{4}:delete` must find the entry stored under the Int 4 (App::RaCoCo's
# DataPart.plus deletes the purple lines this way).
my $b = Hash[UInt, Any].new((4, 1, 9, 1));
is $b{4}:delete, 1, 'delete by Int key returns the value';
is $b.keys.sort.List, (9,), 'the entry is gone';

my $k = 9;
my $c = Hash[UInt, Any].new((4, 1, 9, 1));
is $c{$k}:delete, 1, 'delete through a variable key';
is $c.keys.List, (4,), 'only that entry is gone';

# The key is stored as the Int, so the string "4" is a different key.
my $d = Hash[UInt, Any].new((4, 1));
is $d{"4"}:delete.gist, '(UInt)', 'a Str key does not match an Int key';
is $d.elems, 1, 'and nothing was removed';

# Through a `.map` over the keys, as DataPart does.
my $e = Hash[UInt, Any].new((4, 1, 9, 1));
(4,).map({ $e{$_}:delete }).sink;
is $e.keys.List, (9,), 'delete inside a map block';

# The expression form (no variable to look at).
sub holder { state $h = Hash[UInt, Any].new((4, 1, 9, 1)); $h }
holder(){4}:delete;
is holder().keys.List, (9,), 'delete through an expression';
