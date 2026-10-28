use Test;

plan 9;

my %h = a => 1, b => 2;
is-deeply (%h<a>:p), (a => 1), ':p on a present key';
is-deeply (%h<a>:k), 'a', ':k';
is-deeply (%h<a>:v), 1, ':v';
is-deeply (%h<a>:kv), ('a', 1), ':kv';
is-deeply (%h<a b>:p), (a => 1, b => 2), ':p on a slice';
is-deeply (%h<zz>:p), (), ':p on a missing key is empty';

my @a = 10, 20, 30;
is-deeply (@a[1]:p), (1 => 20), ':p on a positional';

class K { has %.ch; method pairs-of($k) { %!ch{$k}:p } }
is-deeply K.new(ch => {x => 5}).pairs-of('x'), (x => 5), ':p on an attribute hash inside a method';

sub builtin-adverb-lookup(%m, $k) { %m{$k}:v }
is builtin-adverb-lookup(%h, 'b'), 2, ':v inside a sub';
