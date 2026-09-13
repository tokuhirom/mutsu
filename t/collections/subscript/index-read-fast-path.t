use Test;

# The ordinary `@a[$i]` / `%h{$k}` read is answered by a fast path that skips
# the general subscript funnel (issue #8308). The fast path is only allowed to
# short-circuit the shapes whose answer it can produce itself; every other
# shape must still fall through to the general path and answer exactly what it
# answered before. This file pins both halves: the shapes the fast path serves,
# and the shapes it must decline.

plan 35;

# --- the shapes the fast path serves -----------------------------------------

my @a = 10, 20, 30;
is @a[0], 10, 'first element';
is @a[2], 30, 'last element';
my $i = 1;
is @a[$i], 20, 'element through a variable index';
is @a[$i + 1], 30, 'element through a computed index';

my @nested = [1, 2], [3, 4];
is @nested[1][0], 3, 'chained read steps through both levels';

my %h = a => 1, b => 2;
is %h<a>, 1, 'present hash key';
is %h{"b"}, 2, 'present hash key through a string expression';
my $k = 'a';
is %h{$k}, 1, 'present hash key through a variable';

# A List and an Array take the same fast path but differ on a miss, so both
# have to read their present elements correctly too.
is (7, 8, 9)[1], 8, 'list element';

# --- shapes the fast path must decline: out of range -------------------------

is-deeply @a[9], Any, 'out-of-range Array read is Any';
is-deeply (1, 2, 3)[9], Nil, 'out-of-range List read is Nil';
my $neg = -1;
nok @a[$neg].defined, 'negative index is undefined (a Failure)';

# --- shapes the fast path must decline: holes and defaults -------------------

my @holed = 1, 2, 3;
@holed[1]:delete;
is-deeply @holed[1], Any, 'a deleted element reads as the Any hole';

my @dflt is default(-1) = 1, 2, 3;
is @dflt[9], -1, 'is default(...) supplies the out-of-range read';
is @dflt[0], 1, 'is default(...) leaves a present element alone';

my Int @typed;
@typed[2] = 7;
is-deeply @typed[0], Int, 'an unwritten typed-array slot reads as the type object';
is @typed[2], 7, 'a written typed-array slot reads its value';

my @shaped[3];
is-deeply @shaped[1], Any, 'an unwritten shaped slot reads as Any';
@shaped[1] = 5;
is @shaped[1], 5, 'a written shaped slot reads its value';

my @native = array[int].new(4, 5, 6);
is @native[1], 5, 'a native array reads through its own storage';

# --- shapes the fast path must decline: element containers -------------------

my $cell = 5;
my @bound = 1, 2, 3;
@bound[1] := $cell;
$cell = 99;
is @bound[1], 99, 'a := bound element tracks the container it was bound to';

my $hcell = 'x';
my %hbound;
%hbound<k> := $hcell;
$hcell = 'y';
is %hbound<k>, 'y', 'a := bound hash entry tracks its container';

# --- shapes the fast path must decline: index coercion -----------------------

is @a["1"], 20, 'a positional string index numifies';
is @a[True], 20, 'a Bool index numifies to 0/1';
is @a[1.9], 20, 'a fractional index truncates';
is-deeply @a[0, 2], (10, 30), 'a comma-list index is a slice';
is-deeply @a[0 .. 1], (10, 20), 'a Range index is a slice';
is-deeply @a[*], (10, 20, 30), 'a Whatever index is every element';

# --- shapes the fast path must decline: hashes with a key type ---------------

my %objh{Int} = 1 => 'one', 2 => 'two';
is %objh{1}, 'one', 'an object hash looks its key up by .WHICH';
is-deeply %h<zz>, Any, 'a missing hash key is Any';
my %hdflt is default(0) = x => 1;
is %hdflt<nope>, 0, 'is default(...) supplies a missing hash key';
is %hdflt<x>, 1, 'is default(...) leaves a present key alone';

# --- shapes the fast path must decline: a user subscript operator ------------

class Boxed {
    has @.items;
}
multi sub postcircumfix:<[ ]>(Boxed $b, Int $n) { $b.items[$n] * 100 }
my $boxed = Boxed.new(items => [1, 2, 3]);
is $boxed[1], 200, 'a user postcircumfix:<[ ]> still intercepts the subscript';

# An Instance target never reaches the fast path, so its AT-POS protocol is
# untouched.
class Countdown {
    method AT-POS($n) { 10 - $n }
}
is Countdown.new[3], 7, 'AT-POS dispatch is unaffected';

# A Junction index still autothreads rather than being read as one element.
ok (@a[0 | 1] == 10).so, 'a Junction index autothreads over the subscript';
