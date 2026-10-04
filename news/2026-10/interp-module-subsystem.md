# Module loading and import/export state leave `Interpreter`

The eleventh subsystem extraction under ADR-10779 (#10779) is the largest one.
It moved 69 fields into `ModuleState` (`src/runtime/module_state.rs`):

- the module search paths, the repository state and the loaded-module table;
- what a module load tracks: the compunit and package stacks, module-owned
  exports and types, and the module's top-level scope;
- the export tables, the import tables and the operator-import units;
- the current distribution and each package's distribution;
- the lexical pragmas: `use strict`, `use fatal`, `use MONKEY-TYPING`,
  `use attributes` and the precompilation switch.

Code reaches them as `self.module.<field>`. A spawned thread keeps what was
learned at load time, such as the loaded modules, the export and import tables
and the pragmas. Its per-load stacks and in-flight `use` state start empty.
`ModuleState::new` and `ModuleState::fork_for_thread` hold exactly the entries
that `Interpreter::new` and `clone_for_thread` used to spell out field by
field, comments included.

Several of these names are also fields of `Compiler` and other structs. Those
accesses were left untouched. `Interpreter` went from 210 to 142 direct fields.
Fields moved; behaviour did not change.
