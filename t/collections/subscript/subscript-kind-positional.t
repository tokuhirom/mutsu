# A subscript's bracket decides which protocol it uses. A value that is not
# Positional is a one-element list holding itself under `[...]` (raku's
# `Any.AT-POS` / `Any.EXISTS-POS`), while `{...}` / `<...>` on the same value
# stays a key lookup. The two cases are indistinguishable from the index's
# runtime type alone, so the compiler records the bracket on the subscript ops.
#
# Every assertion here also passes unmodified under rakudo.
use Test;
plan 31;

# --- :exists through a positional subscript on an Associative container ---
my $c = { a => 1 };
is-deeply ($c[0]:exists), True,  'hash in a scalar: [0] exists';
is-deeply ($c[1]:exists), False, 'hash in a scalar: [1] does not exist';
is-deeply ($c[0]:!exists), False, 'hash in a scalar: :!exists negates';
is-deeply ($c<a>:exists), True,  'hash in a scalar: <a> is still a key lookup';
is-deeply ($c{0}:exists), False, 'hash in a scalar: {0} is a key lookup, not positional';

my %h = 0 => "x";
is-deeply (%h{0}:exists), True, 'numeric key lookup is unaffected';
is-deeply (%h[0]:exists), True, 'a hash under [0] is a one-element list';
is-deeply (%h[1]:exists), False, 'a hash under [1] does not exist';

my $s = <a b>.Set;
is-deeply ($s[0]:exists), True,  'Set in a scalar: [0] exists';
is-deeply ($s[1]:exists), False, 'Set in a scalar: [1] does not exist';
is-deeply ($s<a>:exists), True,  'Set in a scalar: <a> is still a key lookup';

my $b = bag(<a a b>);
is-deeply ($b[0]:exists), True, 'Bag in a scalar: [0] exists';
is-deeply ($b<a>:exists), True, 'Bag in a scalar: <a> is still a key lookup';

my $m = (a => 1.5).Mix;
is-deeply ($m[0]:exists), True, 'Mix in a scalar: [0] exists';

# --- Any.EXISTS-POS as a method ---
is-deeply $c.EXISTS-POS(0), True,  'Hash.EXISTS-POS(0)';
is-deeply $c.EXISTS-POS(1), False, 'Hash.EXISTS-POS(1)';
is-deeply %h.EXISTS-POS(1), False, 'Hash.EXISTS-POS past the one-element list';
is-deeply $s.EXISTS-POS(1), False, 'Set.EXISTS-POS past the one-element list';

# --- value adverbs on a positional scalar subscript ---
my $i = 5;
is-deeply ($i[0]:kv), (0, 5),      ':kv on a scalar subscript';
is-deeply ($i[1]:kv), (),          ':kv past the one-element list';
is-deeply ($i[0]:p),  (0 => 5),    ':p on a scalar subscript';
is-deeply ($i[0]:k),  0,           ':k on a scalar subscript';
is-deeply ($i[0]:v),  5,           ':v on a scalar subscript';
is-deeply ($i[1]:v),  (),          ':v past the one-element list';

my $t = "abc";
is-deeply ($t[0]:v), "abc", ':v reads a Str as a one-element list, not by character';

is-deeply ($c[0]:v), {a => 1}, ':v on a hash under a positional subscript is the hash';
is-deeply ($c[0]:k), 0,        ':k on a hash under a positional subscript';
is-deeply ($c<a>:v), 1,        ':v under an associative subscript is still a key lookup';

# --- an instance with no EXISTS-POS of its own ---
class Plain { has $.x }
my $o = Plain.new(x => 1);
is-deeply ($o[0]:exists), True,  'instance without EXISTS-POS: [0] exists';
is-deeply ($o[1]:exists), False, 'instance without EXISTS-POS: [1] does not exist';

class WithPos { method EXISTS-POS($i) { $i < 3 } }
is-deeply (WithPos.new[2]:exists), True, 'a class EXISTS-POS is not shadowed';
