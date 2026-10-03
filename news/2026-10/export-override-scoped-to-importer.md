# A `sub EXPORT` override stays out of the exporting module

JSON::Fast::Hyper imports JSON::Fast and exports, under the same names, a
`proto to-json` whose candidate calls `to-json` per element. Raku resolves that
inner call lexically, to JSON::Fast's routine. In mutsu the module's own call
reached the override it had just exported, so nested arrays were wrapped
recursively and `from-json` failed on the result.

Two things were wrong, and both are fixed:

- **The override applied everywhere.** An EXPORT-installed `&name` override now
  applies only to code in the units that ran the `use`. Code of any other unit,
  the exporting module's own routines above all, gets what that unit itself
  imported under the name.
- **Code was attributed to the wrong unit.** A chunk compiled after loading
  could be stamped with the unit of the code that triggered the compile rather
  than the unit it was written in, so the first check could not tell them apart.
  This covered a recompiled `.map`/`.grep` block pulled from the script, a
  routine body compiled on first call, and a module parsed before its unit was
  entered. The unit guard is now entered before the compiler builds the chunk,
  and from the block's own origin.

JSON::Fast::Hyper's test file passes 5/5.
