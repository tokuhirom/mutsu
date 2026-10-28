# A nearer-but-wider multi candidate no longer prunes a narrower one

The narrowest-first scan of a multi's candidates stopped at a candidate that "ranked ahead" of the
next ones. The rank leads with the summed argument distance, which an unrelated first parameter can
shorten (a role `does`-mixed into the argument instance sits one step from a candidate typed with
that role), so `(ElementWrapper, Attribute, Mu)` ranked ahead of `(TypedNode, SoA, $obj where ...)`
though the latter is narrower on its second and third parameters; rakudo needs a candidate to be at
least as narrow on every parameter. The rank order now counts "ahead" as narrower only when the
other candidate does not win a declared parameter. XML::Class's `t/090-types.t` (its `deserialise`
family) passes 60/60.
