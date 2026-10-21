# A unit's top-level `use` fills lexical import sets

The main program's top-level `use`s now open lexical import sets, as a block's `use` does
(ADR-8751 stage 1, #14140 item 3 step 1). What a top-level `use` imports binds in the unit's
set; the call sites it encloses read it through `LexImportCall`. `use Test` and the pragmas
keep the registry path (they have no set), and so do the multi families of a top-level `use`
(see #14361).

- `ModuleState::unit_lexical_sets` holds the sets of a unit's own `use`s, because no import
  scope is open at the top level. The run-time readers (`MY::`, `::('&name')`, `EVAL`, an
  lvalue call such as `substr(...) = ...`) read the running unit's sets after every open
  scope's. A regex code block compiles under the running code's sets.
- The top-level `use` binds from its line on: mainline code above it, such as `&ok` in a block
  that `use`s Test above a later `use` of another `ok`, keeps meaning the name it meant. The
  bodies of routines and methods, which are late-bound, see the set.
- Method and role method bodies are compiled under the import sets around the class.
- A sub the scope declares itself still wins over an import of the same name, now decided at
  compile time per call site; it also holds in a block, where `use M <f>; sub f` used to call
  the import.
- A `sub EXPORT` hook's value for a name wins over a same-named routine a module the hook
  `use`d for itself left in the capture. A hook's family of a built-in operator keeps its env
  alias, so a call none of its candidates accepts falls back to the operator.
- A routine a trait wrapped (`is native`) runs its wrapper when called through an import set.
- `&name.candidates` of an imported multi family reports each declaration once.
- `MY::` lists a top-level hook routine; an exported `unit role A::B::R` is found by its
  short name through the module that is the type; a hook's `&MAIN` stays an env binding.
