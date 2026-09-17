# Manual `EXPORT::<tag>` stash: `our sub`/custom operators without `is export` now parse and import

Part of the #7988 `859ab33e` parse-error-expectation-dump cluster (99 distributions
originally, now down to 36).

Raku has a well-known "manual EXPORT stash" idiom for exporting routines without an
`is export` trait on each one: declare them directly inside a module's own
`my package EXPORT::<tag> { ... }` block, `our`-scoped. Every symbol placed there becomes
part of that tag's export list by construction — this is how `Net::IP::Parse` (fez, 0.0.7)
exports its custom `infix:<< ip== >>` / `infix:<< ip<= >>` / `infix:<< ip>= >>` comparison
operators:

```raku
unit module Net::IP::Parse;

my package EXPORT::DEFAULT {
    class IP { ... }

    our sub infix:<< ip== >> (IP:D $lhs, IP:D $rhs --> Bool:D) {
        return cmp($lhs, $rhs) && so ($lhs.octets Z== $rhs.octets).all;
    }
}
```

mutsu never recognized this idiom for routines (it already handled the sibling
`OUR::{'&name'} := &name` re-export binding form, and it already made a class declared the
same way visible as a type). Two independent gaps stacked:

- **Parse time**: the module-export scanner (`collect_exported_subs` in
  `src/parser/stmt/simple/module_exports.rs`) only looked for an explicit `is export` trait,
  so an importing file never learned the operator symbol existed — `$a ip== $b` failed to
  parse at all, with the generic `Confused. expected statement: expected use statement or
  import statement or no statement or ...` message this whole cluster shares.
- **Run time**: even once parseable, `exec_register_sub_op`
  (`src/vm/vm_register_sub_ops.rs`) only recorded an export when the `is export` trait was
  present. The sub was registered under the literal `EXPORT::DEFAULT::name` registry key, but
  `import_module` resolves an export by `{module}::{name}` — so the routine was both
  unparseable *and*, once that was fixed, unresolved at call time ("Unknown function").

Both are fixed generally, not special-cased to this one operator or distribution:

- `collect_exported_subs` now tracks whether it is walking directly inside a `Package` node
  whose name matches the `EXPORT::<tag>` / `Foo::EXPORT::<tag>` shape, and treats every
  `our`-scoped `SubDecl` found there (tagged via the parser's existing `__our_scoped` custom
  trait) as an export, whether or not it also carries `is export`.
- A new `Interpreter::export_implicit_stash_sub` (next to the existing
  `register_our_code_alias`, which handles the binding-based half of the same idiom) aliases
  the installed definition to the `{module}::{name}` registry key `import_module` actually
  looks up, then records the export — mirroring exactly how the binding-based re-export
  already worked.

Pinned by `t/modules/import-export/export-stash-manual-our-sub.t`, against a new fixture
module (`t/lib/ManualExportStashMod.rakumod`) exercising both a plain `our sub` and a custom
infix operator declared this way. Measured against `raku` v2026.07.

`Net::IP::Parse`'s `t/basic.rakutest` moves from dying at line 11 (0/102 assertions reached)
to running 23 assertions before hitting a separate, unrelated bug — a shaped/typed array
slice-assignment that eagerly materializes an infinite lazy RHS (`@a[^N] = (loop { ... })`),
filed as [#8633](https://github.com/tokuhirom/mutsu/issues/8633). The distribution stays
`red` overall pending that fix, but the cluster this PR targets shrank from 37 to 36
distributions (473 → 371 rakudo assertions still unexplained).
