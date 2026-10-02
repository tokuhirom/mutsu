# Package stash writes visible from every frame; user `.not`/`.so` beat the Bool fast path

Taken from the Logic::Ternary ecosystem distribution, where 4 of 5 test files now pass under mutsu
(`t/04-export` remains, see #11062).

- `Pkg::<Name>` is now a stash lookup for every package, not a qualified type name, so a value stored with
  `Pkg::<Name> = v` or `Pkg::{$k} = v` is found again.
- Those stash writes now also go to the `our` store, so they are visible from other frames (subs, methods) instead of
  only the assigning frame.
- `.not` / `.so` call a user-defined method of that name even when the class also defines `Bool`.
