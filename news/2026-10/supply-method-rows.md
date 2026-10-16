# Supply's methods are rows of the method table

The methods of a `Supply` (`map`, `grep`, `tap`, `head`, `batch`, `throttle`, `zip`, `merge`, `unique`, `lines` and
the rest of the 50 that mutsu implements of Rakudo's 63) are now rows of `Supply`, found by their owner (ADR-11276
§9.130). A dozen places that each asked "is this a `Supply`?" before answering by name are gone, and a variable and a
temporary receiver now reach the same routine. `Supply.serialize` and `Supply.sanitize` exist now and answer the
supply itself.
