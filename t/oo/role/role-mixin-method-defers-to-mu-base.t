use Test;

# A role method that defers (`nextwith` / `callsame`) out of a value the role
# is mixed into reaches the `Mu` base candidate, not the role's own method
# again. From Dan's `DataFrame.clone` (`nextwith(:data(@!data.map(*.clone)),
# ...)` on a punned role), whose `grep` then died on `.data` of `Any`.

plan 10;

role R {
    has @.data;
    has $.v = 3;
    method clone { nextwith( :data(@!data.map(*.clone)), :v(5), |%_ ) }
}

# A punned role: its instance wraps an instance of the pun class `R`.
my $p = R.new(data => [1, 2]);
my $pc = $p.clone;
isa-ok $pc, R, 'clone of a punned role keeps the role';
is $pc.v, 5, 'nextwith out of the role clone reaches Mu.clone with its args';
is-deeply $pc.data, [1, 2], 'a list passed to an @ attribute becomes an Array';
is $p.v, 3, 'the original is untouched';

# The same through `but`.
class K { has $.w = 1 }
my $k = K.new but R;
my $kc = $k.clone;
is $kc.v, 5, 'clone of a `but R` value runs the role clone, then Mu.clone';
is $kc.w, 1, 'the inner class attribute survives the clone';

# The role clone runs exactly once.
my $calls = 0;
role Counted { method clone { $calls++; callsame } }
my $c = Counted.new;
$c.clone;
is $calls, 1, 'a punned role clone deferring with callsame is not re-entered';

# Other Mu base candidates behind a role method.
role S {
    method Str  { 'S:' ~ callsame }
    method gist { 'G:' ~ callsame }
}
ok S.new.Str.starts-with('S:S<'), 'callsame out of a role Str reaches Mu.Str';
ok S.new.gist.starts-with('G:S.new'), 'callsame out of a role gist reaches Mu.gist';
ok (K.new but S).gist.starts-with('G:K+{S}.new'), 'callsame out of a `but` role gist reaches Mu.gist';
