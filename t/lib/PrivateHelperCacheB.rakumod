# The same-named private helpers as PrivateHelperCacheA, with different
# answers. See t/modules/module-private-sub-call-cache-per-unit.t.

sub absent(Mu \candidate --> Bool) { candidate =:= Nil }

sub helper($n) {
    my @seen = $n, $n;
    @seen.map({ $_ + 1 }).sum;
}

sub b-helper($n) is export { helper($n) }

sub b-absent-any() is export { absent(Any) }
