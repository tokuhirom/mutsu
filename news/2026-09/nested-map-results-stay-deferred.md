# Nested map results stay deferred

Consuming or sinking an outer `.map` now leaves any `.map` Seqs returned by its callback unevaluated. Readers that need the nested elements, such as rendering and `.flat`, reify them when read. This matches Rakudo when an inner callback has side effects.
