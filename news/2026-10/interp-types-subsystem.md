# The type registry and declaration state leave `Interpreter`

The twelfth subsystem extraction under ADR-10779 (#10779) moved 41 fields into
`TypeState` (`src/runtime/type_state.rs`):

- the type registry and its write generation;
- container and instance type metadata;
- the class, role, enum and subset declaration state: the class being
  defined or constructed, attribute defaults, lexical and package-scoped type
  names, reblessing and the subset caches.

Code reaches them as `self.types.<field>`. A spawned thread still sees the
parent's declarations through copy-on-write snapshots of the registry and the
instance metadata, and it starts the in-flight declaration state fresh.

This step also settled the ADR's open question about where the current package
belongs. `current_package_sym` names the package of the code that is running.
The VM switches it on every method dispatch and restores it on return, just as
it does the routine stack. So it belongs to the frame core and stays a direct
field of `Interpreter`.

`Interpreter` went from 142 to 102 direct fields. With this, every bounded
subsystem has been extracted. What is left is the frame core and the call-site
side channels, which the ADR turns into explicit parameters next. Fields moved;
behaviour did not change.
