use Test;

# A sigil alias on an atom with an unspaced `**N` count: the source-tree
# parser took only the first `*` as the alias's quantifier, turning
# `$<a>=x**2` into `(x*)*` followed by a literal `2` (#9198).

plan 12;

my $m = "xx" ~~ /$<a>=x**2/;
ok $m, '$<a>=x**2 matches "xx"';
is ~$m<a>, 'xx', '... and captures both x';

$m = "xx2" ~~ /$<a>=x**2/;
is ~$m, 'xx', 'the count is not left behind as a literal 2';
is ~$m<a>, 'xx', '... and $<a> is xx';

$m = "xx" ~~ /$<a>=[x]**2/;
is ~$m<a>, 'xx', '$<a>=[x]**2 captures the repeated group';

$m = "xx" ~~ /$<a>=(x)**2/;
is $m<a>.elems, 2, '$<a>=(x)**2 yields one capture per repetition';
is $m<a>.map(~*).join(','), 'x,x', '... each an x';

$m = "xx" ~~ /$<a>=<?before x>**2/;
is-deeply $m.keys.sort.List, <a before>, '$<a>=<?before x>**2 publishes a and before';

$m = "xxx" ~~ /$<a>=x**1..2/;
is ~$m<a>, 'xx', 'a ranged count still works';

$m = "xxx" ~~ /$<a>=x**?1..2/;
is ~$m<a>, 'x', 'a frugal ranged count takes the minimum';

$m = "xx" ~~ /$<a>=x+?/;
is ~$m<a>, 'x', '$<a>=x+? is frugal, not x+ then an optional capture';

$m = "xx" ~~ /$<a>=x*/;
is ~$m<a>, 'xx', 'a single-character quantifier is unchanged';
