# Nested closures mis-attributed to the caller's file; `%?RESOURCES` collided across bare modules under the generic GLOBAL package

Fixed the root cause behind the `t/http-router.rakutest` (vendored Cro::HTTP
suite) "resources" block: 20 subtests (`Get index.html from resources`,
`resource sets correct status code`, etc.) all 404'd. Two compounding, general
interpreter bugs, neither specific to Cro:

1. **A closure/block literal's `source_file` was computed from the
   dynamically-scoped `?FILE` env var, not the file it was lexically written
   in.** `?FILE` only tracks the module *currently being loaded*
   (`run_modules.rs` scopes it for the duration of a module's own mainline).
   A nested closure literal that lives inside an already-loaded module's sub
   is (re)constructed via `MakeAnonSub`/`MakeAnonSubParams` each time that sub
   actually *runs* — by which point `?FILE` has long since reverted to
   whatever file is calling it. So `route { resources-from %?RESOURCES; ... }`
   (the block passed to `Cro::HTTP::Router`'s `route` sub, itself invoked from
   inside a module's exported `sub`) had its `source_file` mis-attributed to
   the caller's own script instead of the module it was written in.

   Fixed by using the existing `executing_source_file()` helper (already used
   for backtrace rendering) instead of `current_source_file()` when
   constructing a closure's `SubData` — it reads the file baked onto the
   innermost enclosing *routine* frame's own `def_file`, which stays correct
   regardless of who is calling, falling back to the dynamic lookup only at
   true mainline scope.

2. **`%?RESOURCES` resolution's frame-walk preferred a `package -> distribution`
   hashmap keyed by the generic `"GLOBAL"` package name.** A module file with
   no `unit module` declaration compiles its top-level subs under that
   generic package (not a name of its own), so `package_distributions["GLOBAL"]`
   is last-loaded-module-wins: loading *any other* bare module afterward
   clobbers the entry, and every bare module's routine then resolves
   `%?RESOURCES` against whichever module happened to load last — not its
   own distribution. `TestModule.rakumod` (the vendored Cro::HTTP test
   fixture) and several `Cro::HTTP::*` submodules loaded transitively by
   `use Cro::HTTP::Server` are all in this shape, so the moment the "resources"
   test block ran after the earlier "multipart/urlencoded/json destructuring"
   block (which pulls in `Cro::HTTP::Server`), the collision hit.

   Fixed in `build_resources_for_package` (`src/runtime/run_dist.rs`) by
   preferring a routine frame's own `def_file` (resolved via the existing
   `detect_distribution` — the same META6.json-walking logic module loading
   itself uses) over the package-keyed hashmap, falling back to the old
   package-based lookup unchanged when a frame carries no `def_file` (method
   frames, `EVAL`'d code, etc.) — so it strictly adds a more precise path
   rather than replacing the existing one.

Pinned by `t/nested-closure-resources-file-attribution.t` (two bare, no-`unit
module` fixture distributions under `t/lib/ResBareA` and `t/lib/ResBareB`,
confirmed to fail with the exact `No such method 'slurp' for invocant of type
'Any'` shape before the fix). `t/http-router.rakutest`'s "resources" block:
20/20 now pass (was 0/20); the file's remaining failure (`The around blocks
are called in top-to-bottom order`) is an unrelated closure-mutation-ordering
bug, not part of this fix.

`todo/tickets/static-resource-content-type-mismatch-and-related-failures.md`
resolved and removed.
