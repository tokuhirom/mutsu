# An operator bound into the EXPORT stash with a literal key is importable

Data::Record exports its lifting operator by binding it straight into the
export stash:

```raku
my package EXPORT {
    package DEFAULT {
        OUR::«'&infix:<@~~>'» := Data::Record::Lifter;
    }
}
```

The run-time half already worked -- `&infix:<@~~>(1, 2)` resolved in the
importer -- but the importer's *parser* never learned the operator, because
it learns a module's exports from a static scan of the module source, and that
scan knew only `is export` declarations and (since #8634) `our sub`s declared
directly in an `EXPORT::<tag>` package. So `use Data::Record` failed to parse
`$value @~~ @fields.AT-POS($key)`, and a plain `say 1 @~~ 2` did worse: it
parsed as something else and silently printed `1`.

The scan now treats `OUR::{'&name'} := ...` / `OUR::«'&name'» := ...` with a
literal key, directly inside an export stash, as exporting `name`. It also
tracks the enclosing package path, so the nested `package EXPORT { package
DEFAULT { ... } }` spelling is recognised as the `EXPORT::DEFAULT` stash just
like the one-line form (previously only the latter was). A key computed at run
time (Moneys' currency postfixes) cannot be seen by a static scan; that is
#9500.

Data::Record's modules now parse and reach the next, independent blocker: a
`method` declared inside a `do { }` block of the class body is not installed
(#9525). Pinned by `t/modules/import-export/export-stash-our-bind-literal.t`
(#9499, part of #7988).
