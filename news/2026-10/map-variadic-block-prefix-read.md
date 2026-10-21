# A `.map` with a slurpy-parameter block reads an infinite source lazily

`(1..*).map(-> *@a { ... }).head(1)` used to hang. A deferred `.map` whose
block has a slurpy parameter was always read in full, so the callback ran
over the whole source. Such a block takes its required positionals per call,
at least one element, so its prefix reads are now cut into groups of that
size, like any other map block. Only a `.grep` with a slurpy block is still
read in full (#14300).
