# `CArray.list` reads its elements without a closure call per element

`CArray.list` (and so `.Array`) pushes `self.AT-POS($i++)` for every element,
and a typed `CArray`'s `AT-POS` answers a native positional reference
(`IntPosRef` / `UIntPosRef` / `NumPosRef`, a `Proxy` whose FETCH is a built-in
routine). Three costs went away:

- `auto_fetch_proxy` reads such a reference's element directly instead of
  dispatching its FETCH through the closure-call machinery (about 7.7k Ir per
  element).
- The straight-line method lane also binds an `Int` argument to an `int` /
  `int64` parameter (the `int $pos` candidate `CArray.list`'s native counter
  selects), where it used to decline every native-integer parameter.
- Building the reference interned five symbols per element; they are interned
  once per process now.

Callgrind slope on a profiling build (second run, N 500 -> 1500), fill +
`.Array` per element: 77.7k -> **71.0k Ir** (136.4k before the straight-line
lane); the fill alone is unchanged at 29.8k.

Refs #16215.
