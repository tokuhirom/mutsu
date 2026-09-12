`Seq.new($iterator).iterator` now returns the original deferred iterator without
draining it to `IterationEnd`, so unbounded user-defined iterators remain lazy.
