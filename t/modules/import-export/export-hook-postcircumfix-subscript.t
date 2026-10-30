use Test;

plan 5;

use lib 't/lib';
use PostcircumfixHook;

my $item = Item.new but Tagged;
my $index = Index.new;

is $item[$index], 'elem 5', 'a subscript uses the postcircumfix family a sub EXPORT hook handed out';
sub via-sub($o) { $o[Index.new] }
is via-sub($item), 'elem 5', 'the same inside a sub';
is ($item,).map({ $_[Index.new] }).head, 'elem 5', 'the same inside a block run later';
is Item.new[Index.new], 'elem 5', 'a plain instance as the invocant';
is-deeply [10, 20, 30][1], 20, 'a plain Array subscript is unaffected';
