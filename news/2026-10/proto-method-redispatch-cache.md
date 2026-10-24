# A `proto method`'s `{*}` redispatch caches its winner

A multi method called through `proto method f(|) { {*} }` re-ranked every
candidate pairwise on every call (`pick_method_winner` -> `assign_tiers`,
O(candidates^2) type-relation walks): 54 ms per call with 200 candidates,
against 27 us for the same multis without the proto, which resolve through
the type-keyed `multi_resolve_cache`. The redispatch now consults that cache
too, under a key that adds the governing owner and the receiver's
definedness (`resolve_proto_redispatch_cached`); an ambiguous resolution is
still never cached. #14868
