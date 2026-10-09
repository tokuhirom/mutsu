# A `trait_mod:<is>` that `.wrap`s a `proto method` wraps the dispatcher

The code object a `proto method` hands to its own `is` trait carries the class and
method name but no candidate index, so `.wrap` fell through to the sub-level wrap
chain and the wrapper never ran. It now addresses the dispatcher slot, like
`.^lookup('m').wrap` already did. Method::Protected's `is protected` on a proto
method takes its `Lock` this way.
