# Test::Mock runs under mutsu

mutsu now passes all ten baseline files in Test::Mock 1.8. The compatibility work covers
placeholder `where` matchers, dynamic metamodel identity, method-wrapper invocants, rw callback
containers, and concurrent lazy initialization of MOP attributes used for per-object locks.
