# Own `is export` subs stay visible when a module also re-exports a stash

A module that both declares `is export` subs and binds routines into its
`EXPORT::DEFAULT` stash (`BEGIN EXPORT::DEFAULT::{.key} := .value for
Test::EXPORT::DEFAULT::`) filled `exported_subs`, which made `import_module`
skip merging the module's own exports. Its `is export` subs stayed callable
but vanished from the importer's `MY::` (`MY::<&run>` was false). The own
exports are now always merged. Test::Coverage's `t/01-basic.rakutest` passes.
