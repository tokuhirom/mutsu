use Test;

# `.new` is inherited from `Mu`, so `$x .= new` (`$x = $x.new`) dispatches on
# whatever `$x` CURRENTLY holds, not on its declared type. The first `.=new`
# on a freshly `my TypeName $x` declaration calls `.new` on the type OBJECT
# and mutsu handled that; a LATER `.=new`, once `$x` holds a populated
# concrete instance, calls `.new` on that VALUE instead, and mutsu's generic
# `.new` fallback had no case for a concrete Set/Bag/Mix value -- only for
# their type objects -- so it died with X::Method::NotFound.
#
# Reduced from `Game::Entities` 0.1.6's `t/entities.t`, which reuses one
# `my SetHash $set .= new;` across several `subtest`s
# (github.com/tokuhirom/mutsu#8496).

plan 6;

my SetHash $set .= new;
isa-ok $set, SetHash, 'first .=new on the type object still works';

$set.set: 'x';
ok $set ~~ set('x'), 'populated before reassigning';

$set .= new;
isa-ok $set, SetHash, '.=new on an already-populated SetHash rebuilds fresh';
ok $set.elems == 0, 'and the rebuilt SetHash is empty';

my BagHash $bag .= new;
$bag{'a'}++;
$bag .= new;
ok $bag.elems == 0, 'the same holds for BagHash';

my MixHash $mix .= new;
$mix{'a'} += 1;
$mix .= new;
ok $mix.elems == 0, '...and MixHash';
