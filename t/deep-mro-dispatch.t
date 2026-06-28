use Test;

# Guards mro_readonly's cached-MRO fast path: for a class whose registry MRO is
# populated, mro_readonly returns class_def.mro directly instead of re-deriving
# it via a BFS. This must stay equivalent for deep hierarchies — method
# resolution, inherited-method order, and constructor BUILD/TWEAK probing (which
# all walk mro_readonly) must behave exactly as the BFS did.

plan 6;

# 4-level linear hierarchy.
class L0 { method tag { 'L0' } method only0 { 'o0' } }
class L1 is L0 { method tag { 'L1' } }
class L2 is L1 { }
class L3 is L2 { method tag { 'L3' } method only3 { 'o3' } }

# 1. Most-derived override wins through a deep chain.
is L3.new.tag, 'L3', 'most-derived override resolves through 4 levels';

# 2. A method inherited from the top is found through the whole chain.
is L3.new.only0, 'o0', 'top-level inherited method resolves through deep MRO';

# 3. A mid-level instance resolves to the nearest ancestor definition.
is L2.new.tag, 'L1', 'mid-level instance resolves to nearest ancestor (L1, skipping L2 gap)';

# 4. .^mro reflects the full linearization order.
is L3.^mro.map(*.^name).head(4).join(','), 'L3,L2,L1,L0',
    '.^mro order is the full C3 linearization';

# 5. Constructor BUILD runs through the MRO (BUILD probing uses mro_readonly).
my $built = '';
class B0 { submethod BUILD { $built ~= '0' } }
class B1 is B0 { submethod BUILD { $built ~= '1' } }
class B2 is B1 { submethod BUILD { $built ~= '2' } }
B2.new;
is $built, '012', 'BUILD submethods run for every class in the MRO';

# 6. isa across the deep chain.
ok L3.new ~~ L0, 'deep-chain instance smartmatches its top ancestor type';
