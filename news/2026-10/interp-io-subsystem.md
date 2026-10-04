# Output, IO handles and TAP state leave `Interpreter`

The tenth subsystem extraction under ADR-10779 (#10779) moved 14 fields into
`IoState` (`src/runtime/io_state.rs`):

- the program output sink and the `warn` suppression state;
- the open IO handle table and the user `IO::Handle` read buffers;
- the program path, the chroot root and the newline mode;
- the encoding registry;
- the TAP (`Test` module) state.

Code reaches them as `self.io.<field>`. `IoState::fork_for_thread` now also
builds what `clone_for_thread` used to build inline for a spawned thread: an
output sink that writes through the parent's shared stdout/stderr buffers, and
a snapshot of the open handles the spawned code can reach.

The ADR had listed the declarator-doc fields (`doc_comments`,
`doc_comment_list` and the two `.WHY` caches) under `io`. They are
per-compilation-unit declaration metadata, so they were re-classified to
`module` and moved into their own `DeclaratorDocs` holder. A module load
used to save and restore them field by field; it now clones one value.

`Interpreter` went from 226 to 210 direct fields. Fields moved; behaviour did
not change.
