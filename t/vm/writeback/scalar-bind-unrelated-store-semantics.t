use Test;

# A scalar `:=` binding used to arm three whole-program latches
# (`local_bind_pairs` being non-empty, `SIGILLESS_READONLY_KEY_SEEN` and
# `CLOSURE_META_KEY_SEEN`), each of which pushed EVERY plain scalar store in the
# program onto the full store cascade -- including stores to variables the
# binding cannot reach. Those gates are now asked per slot instead, so the
# stores below take the fast path for the first time.
#
# This file pins the semantics that must survive that, in a file that HAS such a
# binding in scope: if the narrowing were too aggressive, one of these would
# silently write the wrong slot, skip a constraint, or stop propagating.

plan 26;

# The binding whose mere existence used to tax everything else in the file.
my $bind-source = 1;
my $bound-alias := $bind-source;

# --- 1. the binding itself still works, in both directions ----------------
$bind-source = 5;
is $bound-alias, 5, 'write to the bind source is visible through the alias';
$bound-alias = 9;
is $bind-source, 9, 'write through the alias is visible at the bind source';

# --- 2. an unrelated scalar is unaffected by it ---------------------------
my $plain = 0;
$plain = 3;
is $plain, 3, 'plain store to an unrelated scalar';
$plain += 4;
is $plain, 7, 'compound assign to an unrelated scalar';
$plain++;
is $plain, 8, 'post-increment of an unrelated scalar';
$plain--;
is $plain, 7, 'post-decrement of an unrelated scalar';
is $bind-source, 9, 'the bind group is untouched by unrelated stores';

# --- 3. a loop over unrelated scalars, the shape bench-threads-serial uses -
my $sum = 0;
for ^10 -> $i {
    $sum += $i;
}
is $sum, 45, 'accumulator captured by a for block still accumulates';
my $csum = 0;
loop (my $k = 0; $k < 10; $k++) {
    $csum = $csum + $k;
}
is $csum, 45, 'accumulator in a C-style loop still accumulates';
is $k, 10, 'C-style loop counter reached its bound';

# --- 4. the metadata lanes the fast path stands in for --------------------
my Int $typed = 1;
$typed = 2;
is $typed, 2, 'typed lexical accepts a conforming store';
dies-ok { $typed = "not an Int" }, 'typed lexical still rejects a bad store';

my $defaulted is default(42) = 7;
is $defaulted, 7, 'is default() lexical holds its assigned value';
$defaulted = Nil;
is $defaulted, 42, 'is default() still substitutes on a Nil store';

sub counter() { state $n = 0; $n++; $n }
counter();
counter();
is counter(), 3, 'state variable still persists across calls';

# --- 5. readonly-ness still propagates through a bind ---------------------
sub assign-through-bind($ro) { my $c := $ro; $c = 3 }
dies-ok { assign-through-bind(1) }, 'a bind of a readonly param stays readonly';

my $writable = 1;
sub assign-through-writable-bind($p is rw) { my $c := $p; $c = 3 }
assign-through-writable-bind($writable);
is $writable, 3, 'a bind of an `is rw` param stays writable and writes through';

# --- 6. sigilless binds and their chains ----------------------------------
my $root = 1;
my \sigilless := $root;
sigilless = 4;
is $root, 4, 'sigilless alias writes through to its source';
$root = 6;
is sigilless, 6, 'sigilless alias reads its source';

my $chain-root = 1;
my $mid := $chain-root;
my $tail := $mid;
$tail = 8;
is $chain-root, 8, 'a three-name bind group is closed';
is $mid, 8, 'the middle of a bind group sees the write';

my \immutable := 5;
dies-ok { immutable = 1 }, 'a sigilless bind to a literal stays immutable';

# --- 7. a redeclaration must not inherit the previous binding -------------
{
    my $a = 1;
    my $b := $a;
    $b = 2;
    is $a, 2, 'inner bind group works';
}
{
    my $b = 10;
    $b = 11;
    is $b, 11, 'a same-named redeclaration is a plain writable lexical again';
}

# --- 8. the increment of a bind SOURCE still reaches its alias ------------
my $inc-source = 1;
my $inc-alias := $inc-source;
$inc-source++;
is $inc-alias, 2, 'post-increment of a bind source propagates to the alias';
$inc-alias++;
is $inc-source, 3, 'post-increment through an alias propagates to the source';
