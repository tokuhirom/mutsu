# Qualified names built once per symbol in runtime methods and registration

The #11507 sweep removed all 130 run-time package-name string surgery sites
in two parts of `src/runtime/`: the `[f-q]*.rs` top-level files
(`hoist_visibility`, `main_args`, the `methods_*` family, `nativecall*`,
`nqp_create`) and every `registration_*.rs` file. Totals went from qualify 97
to 41, global-cmp 25 to 12 and scan 135 to 74.

Most sites built `"<current package>::<name>"` with `format!`, so the
interpreter gained `current_package_qualified`, which hands back the
memoized `&'static str`. It also gained `current_package_is_global_name`.

`src/qualified.rs` gained three helpers:

- `qualified_text`, which takes either half as text, a `Symbol` or a `Cow`
  through a small `NamePart` trait;
- `is_global_name`;
- `split_first` (the memoized `split_once("::")`).

Private-method qualifiers (`Owner::name`), compound-name class registration,
role and subset export splitting, and the `Pointer` short-name checks now
all go through them.
