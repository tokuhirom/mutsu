# A package-less private helper that creates a closure (which keeps it off
# every light call path), called repeatedly from this unit's exported subs.
# t/modules/module-private-sub-call-cache-per-unit.t loads this beside
# PrivateHelperCacheB, which declares a private helper of the same name.

my $cache = Map.new;

sub absent(Mu \candidate --> Bool) { candidate =:= Nil || candidate =:= Any }

sub helper($n) {
    my \hit = $cache{$n};
    absent(hit) ?? { $n * 10 }() !! hit;
}

sub a-helper($n) is export { helper($n) }

sub a-many(Int $count) is export { (^$count).map({ helper($_) }).sum }
