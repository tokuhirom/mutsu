use Test;

# ADR-0097 / #8877: a typed lexical's declared constraint is baked onto its
# local slot (`BindingDesc::declared_constraint`) at compile time, so the scalar
# store fast path stops re-deriving it from the `__mutsu_type::` env entry on
# every store.
#
# The bake is only consulted by `native_typed_store_is_identity`, whose one
# question is "would the typed branch leave this value alone?". Every case below
# is a shape where answering that from the declaration instead of the env could
# diverge -- a narrow width that must still wrap, a slot that collects two
# different constraints, a subset that redirects a native name, a trait that
# rewrites the constraint at run time.

plan 19;

# -- the baked natives: the store is the identity, and repeats --
my int $i = 5;
$i = 7;
is $i, 7, 'int slot stores an Int unchanged';
$i = 8;
is $i, 8, 'a second store to the same int slot still lands';

my str $s = "x";
$s = "y";
is $s, "y", 'str slot stores a Str unchanged';

my num $n = 1e0;
$n = 2.5e0;
is $n, 2.5e0, 'num slot stores a Num unchanged';

my int64 $i64 = 1;
$i64 = 9;
is $i64, 9, 'int64 is baked as the same family as int';

# -- a narrow width is NOT the identity: it must still wrap --
my int8 $b = 5;
$b = 300;
is $b, 44, 'int8 still wraps on a store (the bake must not claim identity)';

my int16 $w = 1;
$w = 70000;
is $w, 4464, 'int16 still wraps on a store';

# -- a class constraint still type-checks --
my Int $c = 5;
$c = 9;
is $c, 9, 'Int slot accepts an Int';
dies-ok { my Int $bad = 1; $bad = "nope" }, 'Int slot still rejects a Str';

# -- a native int slot still rejects a value it cannot hold --
dies-ok { my int $ni = 1; $ni = "nope" }, 'int slot still rejects a Str';
dies-ok { my str $ns = "a"; $ns = 42 }, 'str slot still rejects an Int';

# -- one slot, two constraints: the bake must poison and defer to the env.
# In the default build a nested `my $x` reuses the OUTER slot
# (`Compiler::declare_local`), so these two blocks really are one slot.
{ my int $p = 1; is $p, 1, 'first of two same-named typed decls is int'; }
{ my int8 $p = 300; is $p, 44, 'second of two same-named typed decls is int8'; }

# -- typed, then untyped, same name: the untyped declaration clears the
# constraint, and the bake must not resurrect it.
{ my int $q = 1; is $q, 1, 'typed decl of $q'; }
{ my $q = "free"; $q = "still free"; is $q, "still free", 'untyped redecl of the same name takes any value'; }

# -- a subset can redirect a native constraint name, which is why the bake is
# read only after the subset guard.
{
    subset num of Int where * > 100;
    my num $r = 500;
    is $r, 500, 'a subset named `num` redirects the native name';
    dies-ok { my num $r2 = 5 }, 'the redirected `num` enforces its where clause';
}

# -- a trait poisons the slot: `ApplyVarTrait` can rewrite the constraint at run
# time, so the compile-time record cannot describe the slot.
my Int $d is default(42);
is $d, 42, 'is default(...) on a typed slot still applies';

# -- a loop: the point of the whole exercise is that the store repeats
my int $acc = 0;
for 1..100 -> $e { $acc = $acc + $e }
is $acc, 5050, 'a hot typed-native store loop still accumulates correctly';
