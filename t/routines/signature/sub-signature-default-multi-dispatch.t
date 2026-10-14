use v6.d;
use Test;

# Red (`trait_mod:<is>(Attribute, :$referencing! (..., Str :$require = $model))`):
# an omitted sub-signature parameter whose default refers to an earlier
# parameter must not make the multi candidate fail to match while dispatching.

plan 4;

multi f(Int $x, :$a! (Str :$m!, Str :$c!, Str :$r = $m)) { "$m $c $r" }
is f(1, :a{ :m<x>, :c<y> }), 'x y x', 'default referring to an earlier sub-signature param';
is f(1, :a{ :m<x>, :c<y>, :r<z> }), 'x y z', 'supplied value wins over the default';

my @seen;
multi trait_mod:<is>(Attribute $attr, :$refs! (Str :$model!, Str :$require = $model)) {
    @seen.push: "$model $require";
}
class A { has $.x is refs{ :model<M> } }
is @seen.join(','), 'M M', 'attribute trait with such a sub-signature is claimed by the multi';

multi g(:$a! (Str :$m!, Str :$r = $m)) { "$m $r" }
is g(:a{ :m<q> }), 'q q', 'works without a positional';
