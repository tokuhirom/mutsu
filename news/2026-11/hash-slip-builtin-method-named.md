# `|%hash` passes bare named-argument values to built-in methods

A hash slipped into an argument list kept each value inside its `Scalar`, so
built-in methods that match `:p(True)` by value (`@l.max(|%a)`) ignored the
adverb. The slip now hands over the decontainerized value, matching rakudo.
