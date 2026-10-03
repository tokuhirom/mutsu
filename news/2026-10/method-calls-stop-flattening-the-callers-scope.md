# Method calls stop copying the caller's whole scope

Refs [#9494](https://github.com/tokuhirom/mutsu/issues/9494).

`use Services::PortMapping` parses a 15,000-line CSV file with Text::CSV
while the module loads. Every parsed field makes a dozen method calls, and a
`CallMethodMut` dispatch from inside a routine collapsed the routine's
scoped environment into a flat copy of every name in scope before it went
anywhere: a whole-map clone per call, plus a re-index of every key, plus a
scope-sized return merge in the caller for the rest of its life (#7563,
#7630). Sub calls have never done this; their bodies run on a fresh overlay
chained over the caller's.

This round removes the copy from the dispatch shapes that reach nothing
needing the full lexical view, and prunes a few per-call recomputations
around them:

- **User methods through the plain-method lane** run their compiled body
  the way a sub does, without flattening the caller first. The lane's tail
  still flattens before its native and interpreter fallbacks.
- **Private methods** (`self!ready($f)`) resolve through a type-keyed cache
  instead of cloning the class's overload list and the winning `MethodDef`
  on every call, and join the plain-method lane.
- **`.defined` on a type object** (an uninitialized typed attribute) and the
  **plain-array mutators** (`@!fields.push: $f`) are answered before the
  flatten, with the same decisions the full path makes.
- A **no-argument pure native method on a `Str`/`Int`/`Num`/`Bool`**
  (`$chunk.chars`) skips the ~700 lines of receiver probes in front of the
  same gate.
- The method-return merge drops the frame's own fixtures, parameters and
  locals by symbol before running its string predicates.
- A **literal attribute default** (`is default(False)`) no longer builds and
  tears down a `self`/`?CLASS`/per-attribute scope around a constant, and
  whether a class's user `new` declines a no-argument call is memoized on
  its constructor plan.

Measured with callgrind on the IANA port list (a warm cache, instructions per
parsed row, first-100-rows baseline subtracted): **15.27M -> 12.47M (-18%)**.
A cold-cache `use Services::PortMapping` on a 4-core container went from
52.9 s to 45.1 s. That is short of the issue's goal (at most 60 s in the
ecosystem sweep, which measured 126 s), so the issue stays open.
