# An `our proto sub` in a module's EXPORT stash now exports its multi family

The "manual EXPORT stash" idiom (`my package EXPORT::<tag> { ... }`, #7988) already exported an
`our sub`/`our multi sub` declared directly inside it. It did not export an `our proto sub` the
same way — the only legal spelling for a *multi family* in that idiom, since raku rejects `our
multi sub` outright ("Cannot use 'our' with individual multi candidates. Please declare an
our-scoped proto instead"). After `use`, calling the family raised `Unknown function`.

`exec_register_proto_sub_op` (`src/vm/vm_register_sub_ops.rs`) now aliases an `our`-scoped
stash proto into the loading module's namespace the same way `export_implicit_stash_sub` does for
a plain sub (new `Interpreter::export_implicit_stash_proto`, `src/runtime/runtime_module_exports.rs`).
The proto commonly precedes its candidates, and those bare `multi sub` declarations are not
themselves `our`-scoped, so `exec_register_sub_op` now also recognises (via
`Interpreter::is_our_scoped_proto`) that a candidate's proto already put the family in the stash's
export list, and re-aliases the family as each candidate registers. The parser's own export scan
(`collect_exported_subs_in`, `src/parser/stmt/simple/module_exports.rs`) picks up the same case, so
an operator declared this way still parses in the importing file.

Both `EXPORT::DEFAULT` and `EXPORT::ALL` (`use Mod :ALL`) are covered by
`t/modules/import-export/export-stash-manual-our-proto.t`. See #9720.
