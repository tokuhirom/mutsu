# `trait_mod:<is>(..., :SYMBOL, :export)` exports a routine under another name

List::AllUtils 0.0.6 re-exports every routine from List::Util, List::MoreUtils
and List::UtilsBy. It walks each module's `EXPORT::all` stash in a `BEGIN`
block and calls CORE's routine-export trait directly:

```raku
trait_mod:<is>(
  (List::AllUtils::{.key} = .value),
  :SYMBOL(.key),
  :export(:all)
) unless List::AllUtils::{.key}:exists;
```

mutsu's prelude candidate for that trait was `(Routine:D \r, :$export!)`.
Rakudo's has an extra `:$SYMBOL = '&' ~ r.name`, so the call matched nothing
and the module died at load time with "No matching candidates for proto sub:
trait_mod:<is>". Two more gaps sat behind that one:

- **`:SYMBOL` publishes the value.** The candidate now takes `:$SYMBOL`. When
  the name it gives is not the routine's own name, or the routine belongs to
  another package, `__mutsu_routine_export` binds the routine value into the
  current package's `EXPORT::ALL` and `EXPORT::<tag>` stashes, as Rakudo's
  `EXPORT_SYMBOL` does. It no longer re-registers the routine by name. These
  exports go through `publish_package_stash_symbol`, which now also writes the
  module-qualified value to the durable `our_vars` store. Before, the value
  lived only in the module's `env`, which is dropped once the module finishes
  loading, so the import could not find it.
- **Re-exported protos kept their first winner.** A proto that is called
  through its value resolves `List::MoreUtils::apply` by qualified name.
  `compile_and_call_function_def` then cached the winning candidate as the
  meaning of the bare name `apply` at the callsite. Its "is this a multi?"
  check looked at the callsite's view of the name, where no candidates are
  visible, so the check passed. After that, every `apply(...)` ran the first
  winner whatever the argument types. A def resolved through a qualified name
  is no longer cached under its bare name.

List::AllUtils now loads, and both of its test files pass under mutsu as they
do under rakudo (222 of 222 assertions each). The fix is pinned by
`t/modules/import-export/trait-mod-is-export-symbol.t`. Two unrelated bugs
turned up along the way and have their own issues: #10023 (a block's rw `$_`
does not write back into a caller's own `$_`) and #10025 (`Pkg::<&routine>`
stash reads).
