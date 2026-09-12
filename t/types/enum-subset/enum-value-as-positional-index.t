use v6;
use Test;

# An enum value is a `Cool`, so as a POSITIONAL subscript it numifies to its
# value: `@a[Green]` is `@a[1]`, exactly like the explicit `@a[+Green]` and
# `@a[Green.Int]` spellings. An ASSOCIATIVE subscript is different — there the
# enum value is the KEY itself, not its ordinal.
#
# mutsu numified neither: the read path left the enum alone (so `@a[Green]`
# answered `Nil`) and `index_to_usize` fell through to parsing the enum's KEY
# ("Green") as a number, so a write reported "Index out of bounds".
#
# Found via the Business::CreditCard distribution, which maps an obsolete card
# brand to its current one by indexing a table with the card type it just
# looked up (`@renamed[$found]`).

plan 21;

enum Color <Red Green Blue>;

my @a = 'zero', 'one', 'two';

# --- read ---
is @a[Green], 'one', 'enum value as array subscript numifies to its value';
is @a[Red], 'zero', 'first enum value indexes element 0';
is @a[Blue], 'two', 'last enum value indexes the last element';

my $c = Green;
is @a[$c], 'one', 'enum value held in a scalar indexes the same';
is @a[+Green], 'one', 'explicit numification agrees';
is @a[Green.Int], 'one', 'explicit .Int agrees';

# --- a constant array (the shape Business::CreditCard uses) ---
my constant @k = 'zero', 'one', 'two';
is @k[Green], 'one', 'enum value indexes a constant array';

# --- a List ---
my $l = (1, 2, 3);
is $l[Green], 2, 'enum value indexes a List';

# --- slice ---
is-deeply @a[Red, Blue], ('zero', 'two'), 'enum values as a slice';

# --- write ---
my @w = 'a', 'b', 'c';
@w[Green] = 'X';
is-deeply @w, ['a', 'X', 'c'], 'enum value as an assignment subscript';

my @auto;
@auto[Blue] = 'grown';
is @auto.elems, 3, 'enum value autovivifies to its ordinal length';
is @auto[2], 'grown', 'autovivified element landed at the ordinal';

# --- an enum with explicit (non-ordinal) values numifies to the VALUE ---
enum Sized (Small => 3, Big => 5);
my @s = ^8;
is @s[Small], 3, 'explicitly-valued enum indexes by its value, not its position';
is @s[Big], 5, 'explicitly-valued enum indexes by its value (second)';

# --- an associative subscript keeps the enum as the KEY ---
my %h;
%h{Green} = 'g';
is %h{Green}, 'g', 'enum value as a hash key round-trips';
is-deeply %h.keys.List, ('Green',), 'hash key is the enum, not its ordinal';

# --- Bool is `enum Bool <False True>`, so it numifies the same way ---
is @a[True], 'one', 'True as a positional subscript is index 1';
is @a[False], 'zero', 'False as a positional subscript is index 0';
my @bw = 'a', 'b', 'c';
@bw[True] = 'Y';
is-deeply @bw, ['a', 'Y', 'c'], 'True as an assignment subscript';
my %bh;
%bh{True} = 't';
is-deeply %bh.keys.List, ('True',), 'associative subscript keeps the Bool as key';

# --- a STRING-valued enum has no numeric value, so it must NOT fold to 0 ---
enum Stringy (Ess => 'x');
nok (try @a[Ess]).defined, 'string-valued enum does not silently index element 0';
