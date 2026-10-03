# Variable storage outside the frames leaves `Interpreter`

The ninth subsystem extraction under ADR-10779 (#10779) moved 41 fields into
`LexicalState` (`src/runtime/lexical_state.rs`). They hold variable storage and
lexical bookkeeping that live outside the VM frames:

- `our` variables and the package/unit lexical tables;
- `state` variables and their scope ids;
- the escaping-`our` cells and lexical-sub aliasing;
- nested method captures;
- readonly tracking and the per-block declaration sets.

Code reaches them as `self.lexicals.<field>`.

About half of these fields are carried into a spawned thread, most of them as
`Arc` shares. The rest start fresh. `LexicalState::new` and
`LexicalState::fork_for_thread` now hold exactly the entries that
`Interpreter::new` and `clone_for_thread` used to spell out field by field,
comments included.

Several of these names are also fields of other structs, notably
`CompiledCode` and `CompiledSubDeclPlan`. Accesses to those other structs were
left untouched. `Interpreter` went from 266 to 226 direct fields.
Fields moved; behaviour did not change.
