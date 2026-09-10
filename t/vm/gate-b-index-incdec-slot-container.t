use Test;

# Pin for the (B) per-store env-write gate index inc/dec container-read fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# `$c[i]++` / `$h<k>++` (OpCode::PostIncrementIndex / PreIncrementIndex) read the
# base container from `env` by name (`get_env_with_main_alias`) and mutate it in
# place via `env.get_mut(name)`. Under MUTSU_GATE_LOCAL_ENV_WRITE a
# `my $bh = BagHash(...)` writes the local slot but skips the env mirror, so the
# env-first read snapshotted the `my` decl seed (Any/absent): the bump landed in
# env (or nowhere) and the slot's live container never saw it, so `$bh<a>` read
# back as its old weight. The fix reads and mutates the slot's container under the
# gate (the authoritative half). Gate OFF (default) is byte-identical. This passes
# gate-OFF and would fail gate-ON before the fix.

plan 8;

# BagHash weight bump through a scalar-held coerced container.
my $bh = BagHash(<a a>);
$bh<a>++;
is $bh<a>, 3, 'BagHash($x)<k>++ bumps the slot-held container';

# MixHash weight set/bump.
my $mh = MixHash(<a>);
$mh<b>++;
is $mh<b>, 1, 'MixHash($x)<k>++ on a slot-held container';

# SetHash element add via ++ (Bool-valued).
my $sh = SetHash(<a b>);
$sh<c>++;
ok $sh<c>, 'SetHash($x)<k>++ adds the key';
is $sh.elems, 3, 'SetHash gained the element';

# Plain Hash element inc through a scalar container.
my $h = %( a => 1 );
$h<a>++;
is $h<a>, 2, 'scalar-held Hash <k>++';

# Array element inc through a scalar container.
my $arr = [10, 20, 30];
$arr[1]++;
is $arr[1], 21, 'scalar-held Array [i]++';

# Prefix increment on a slot-held container.
my $bh2 = BagHash(<x x x>);
++$bh2<x>;
is $bh2<x>, 4, 'prefix ++$bh<k> on a slot-held container';

# Decrement removes a Bag weight down to 0.
my $bh3 = BagHash(<y>);
$bh3<y>--;
is $bh3<y>, 0, 'BagHash <k>-- clamps to 0';
