# A slang's `meta-object` is one object per node, and a mixed-in routine is assignable through any expression

A slang action that read a declared method's `meta-object` twice got two
different routines: before the declaration runs, the slang host builds a
stand-in routine, and it built a fresh one on every call. So
`$method.meta-object does Tagged; $method.meta-object.tag = ...` mixed the role
into one object and wrote through another. The stand-in is now memoized on
the node, as rakudo memoizes a node's meta-object.

The write itself also failed: a routine that a role was mixed into keeps the
composition in its identity cell, but assigning to a role attribute through
anything other than the variable `does` was applied to (a method's or an
accessor's answer, an array element) died with "cannot assign through .tag on
non-instance". The assignment now reads the routine as the mixin it currently
is (#17415).
