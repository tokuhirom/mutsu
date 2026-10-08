use Test;

# From the Map::Ordered distribution: `my %m is Role` where Role does
# Associative mixes the role into a Hash; the :k/:kv/:p/:v subscript adverbs
# must go through the role's AT-KEY / EXISTS-KEY.

plan 6;

role R does Associative {
    has %!h;
    method STORE(*@v) { for @v -> $p { %!h{$p.key} = $p.value }; self }
    method AT-KEY(\k) { %!h{k} }
    method EXISTS-KEY(\k) { %!h{k}:exists }
    method keys() { %!h.keys.sort }
}

my %m is R = a => 1, b => 2;
is-deeply (%m<a b>:v).List, (1, 2), ":v slice";
is-deeply (%m<a b>:k).List, ("a", "b"), ":k slice";
is-deeply (%m<a b>:kv).List, ("a", 1, "b", 2), ":kv slice";
is-deeply (%m<a b>:p).List, (:a(1), :b(2)), ":p slice";
is-deeply (%m{*}:v).List, (1, 2), "whatever slice with :v";
is-deeply (%m{}:v).List, (1, 2), "zen slice with :v";

done-testing;
