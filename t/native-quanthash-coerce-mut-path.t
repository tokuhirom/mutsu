use Test;

# QuantHash coercion on a *variable* receiver (`@a.Set`, `%h.MixHash`, ...).
# Variable receivers compile to CallMethodMut, so these land on the VM's mut
# dispatch path, which previously fell back to the interpreter for all six
# coercions. They are now native there too (the coercion returns a new value and
# never mutates the receiver variable). Behavior must match the interpreter.

plan 19;

my @a = <a a b c>;

# --- all six coercions go native and keep their type identity --------------
is @a.Set.^name,     'Set',     '@a.Set';
is @a.Bag.^name,     'Bag',     '@a.Bag';
is @a.Mix.^name,     'Mix',     '@a.Mix';
is @a.SetHash.^name, 'SetHash', '@a.SetHash';
is @a.BagHash.^name, 'BagHash', '@a.BagHash';
is @a.MixHash.^name, 'MixHash', '@a.MixHash';

# --- weights / membership (Str elements => keys match subscripts) ----------
is @a.Bag<a>, 2, 'Bag weight from variable receiver';
is @a.Bag<b>, 1, 'Bag weight 1';
ok @a.Set<a>,    'Set membership true';
nok @a.Set<z>,   'Set non-membership';
is @a.MixHash<a>, 2, 'MixHash weight from variable receiver';

# --- hash receiver ---------------------------------------------------------
my %h = x => 1, y => 2;
is %h.Set.^name,     'Set',     '%h.Set';
is %h.MixHash.^name, 'MixHash', '%h.MixHash';
is %h.Bag<x>, 1, '%h.Bag weight';

# --- mutability of the hashy variants -------------------------------------
my $sh = @a.SetHash;
$sh<d> = True;
ok $sh<d>, 'SetHash from variable receiver is mutable';
my $mh = @a.MixHash;
$mh<e> = 4;
is $mh<e>, 4, 'MixHash from variable receiver is mutable';

# --- the receiver variable itself is unchanged (coercion is non-mutating) --
is @a.elems, 4, 'receiver array unchanged after coercion';
is @a.join(","), 'a,a,b,c', 'receiver array contents unchanged';

# --- typed (non-Str) elements keep original key values --------------------
my @nums = 1, 1, 2;
is @nums.Bag{1}, 2, 'Bag of Int elements keyed by value';
