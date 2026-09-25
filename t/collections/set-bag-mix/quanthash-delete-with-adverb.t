use Test;

plan 10;

# `:delete` combined with a return adverb (`:k`/`:v`/`:p`/`:kv`) on a
# SetHash/BagHash/MixHash returned the right value but left the element in
# place. Issue #9335.

for <SetHash BagHash MixHash> -> $type {
    subtest "$type single key", {
        plan 8;
        for <k v p kv> -> $adv {
            my $q = ::($type).new-from-pairs('a' => 2, 'b' => 3);
            my $r = do given $adv {
                when 'k'  { $q<a>:delete:k }
                when 'v'  { $q<a>:delete:v }
                when 'p'  { $q<a>:delete:p }
                when 'kv' { $q<a>:delete:kv }
            };
            my $want-v = $type eq 'SetHash' ?? True !! 2;
            my $got = do given $adv {
                when 'k'  { $r }
                when 'v'  { $r }
                when 'p'  { $r.key ~ '=' ~ $r.value }
                when 'kv' { $r.join('=') }
            };
            my $want = do given $adv {
                when 'k'  { 'a' }
                when 'v'  { $want-v }
                default   { "a=$want-v" }
            };
            is $got, $want, ":delete:$adv returns the removed entry";
            is $q.keys.sort.join(','), 'b', ":delete:$adv removes it";
        }
    }
}

for <SetHash BagHash MixHash> -> $type {
    my $q = ::($type).new-from-pairs('a' => 1, 'b' => 1, 'c' => 1);
    my @k = $q<a b>:delete:k;
    is "{@k.sort} / {$q.keys.sort}", 'a b / c', "$type slice :delete:k removes every key";
}

{
    my $m = ('a' => 1, 'b' => 2, 'c' => 3).MixHash;
    my @out;
    @out.push($m{$m.roll}:delete:k) while $m;
    is @out.sort.join(','), 'a,b,c', 'the List::UtilsBy weighted_shuffle_by loop drains the MixHash';
}

{
    my $b = <a a b>.BagHash;
    is ($b<a>:!delete:k), 'a', ':!delete:k does not delete';
    is $b.elems, 2, '... and the element stays';
}

{
    my $s = set <a b>;
    throws-like { $s<a>:delete:k }, X::Assignment::RO,
        ':delete:k on an immutable Set is refused like plain :delete';
}
