use Test;

# `.MixHash` coercion native dispatch (VM-native, shared impl with the
# interpreter via `builtins::quanthash_coerce::to_mixhash`). Each literal /
# expression receiver goes through the native path; behavior must match the
# interpreter fallback exactly (same value the `.Mix` folding produces, plus the
# mutable flag and embedded `MixHash` type metadata).
#
# Uses Str elements so the angle-bracket subscript (`<a>` = Str "a") looks up the
# same key the element folds to, matching raku's `.WHICH`-keyed semantics.

plan 22;

# --- type identity --------------------------------------------------------
my $a = <a a b c>.MixHash;
isa-ok $a, MixHash, 'List.MixHash is a MixHash';
is $a.^name, 'MixHash', '.^name is MixHash';
ok $a ~~ MixHash, 'smartmatches MixHash';
ok $a ~~ Mixy, 'MixHash does Mixy';

# --- weights / total ------------------------------------------------------
is $a<a>, 2, 'duplicate element has weight 2';
is $a<b>, 1, 'single element has weight 1';
is $a<c>, 1, 'single element has weight 1 (c)';
is $a.total, 4, 'total weight is 4';
ok $a.so, 'a non-empty MixHash is truthy';

# --- mutability (MixHash is the mutable QuantHash) ------------------------
my $m = <x y>.MixHash;
$m<z> = 5;
is $m<z>, 5, 'MixHash element is assignable';
is $m.elems, 3, 'assignment grows the MixHash';
$m<x> = 0;
is $m.elems, 2, 'setting weight to 0 removes the element';

# --- from various receiver shapes ----------------------------------------
is (a => 1.5, b => 2).Mix.MixHash.^name, 'MixHash', 'Mix.MixHash';
is <a b b>.Bag.MixHash.^name,             'MixHash', 'Bag.MixHash';
is <x y x>.Set.MixHash.^name,             'MixHash', 'Set.MixHash';
is ('a', 'b', 'c').MixHash.^name,         'MixHash', 'List.MixHash';
is (a => 1).MixHash.^name,                'MixHash', 'Pair.MixHash';
my %h = a => 1, b => 2;
is %h.MixHash.^name, 'MixHash', 'Hash.MixHash (variable receiver)';

# --- empty ----------------------------------------------------------------
my $e = ().MixHash;
is $e.^name, 'MixHash', 'empty List.MixHash is a MixHash';
nok $e.so, 'an empty MixHash is falsy';

# --- Mix (immutable) is unaffected ---------------------------------------
my $im = <a a b>.Mix;
is $im.^name, 'Mix', '.Mix is still immutable Mix';
is $im<a>, 2, 'Mix weights unchanged';
