Native and boxed numeric multi-dispatch candidates now distinguish their
argument provenance. Boxed values no longer match native-only candidates, and
native candidates with different widths in the same family correctly report
an ambiguous dispatch.
