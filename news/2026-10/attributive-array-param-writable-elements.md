# An attributive `@` parameter binds a List argument with writable elements

`submethod TWEAK(:@!env) { for @!env { $_ = ... } }` died with "Cannot assign to an
immutable value" when the argument was a List, because the method fast path
stored the List itself in the attribute. An attributive `@` parameter assigns its
argument into the attribute's Array (`@!env = |$arg`), so the elements are fresh,
writable containers. Both binders now share one helper
(`attributive_array_param_value`) that re-homes a List or Slip into a real Array.
SBOM::CycloneDX's `SBOM::Input`/`SBOM::Output` rely on this. Closes #12741.
