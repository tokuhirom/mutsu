# Constrained type construction in v6.c declarations

An `.= new` initializer on a typed scalar now uses the definiteness-constrained
type as its invocant under `use v6.c`. Constructing that type raises Rakudo's
error. Under v6.d and later, the initializer continues to use the base type.
