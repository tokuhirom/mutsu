# `.pick(N).iterator` is not deterministic

The Seq returned by `.pick` is marked as having non-reproducible elements, so
its `.iterator` answers `False` to `is-deterministic`, as on rakudo. This
completes the iterator-protocol table of #16941.
