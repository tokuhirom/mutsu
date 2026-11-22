# Exported trait_mod multis that tie are pinned to source order

Two `multi trait_mod:<is>` candidates of a module that tie on every ranking
key except declaration order — Red's `(Red::Attr::Column $attr, :%column!)`
and `(Attribute $attr, :%column!)` — are tried in source order, as rakudo
does. Red's `t/095-indexes.rakutest` once depended on the build here (#18046);
it no longer reproduces, and `t/modules/multi-tie-source-order-module-traits.t`
now pins both declaration orders, called directly and through the traits of a
second module that loads the same candidates.
