# `Version.parts` lists a `*` part as the Str "*"

`Version.new('1.2.*').parts` was `(1, 2, *)` with a `Whatever` last element; Rakudo's list holds the
string `"*"` (`(1, 2, "*")`, `.WHAT` is `Str`, `~~ Whatever` is `False`). The `parts` row
(`src/builtins/method_table/scalars/version.rs`) mapped `VersionPart::Whatever` to `Value::WHATEVER`
and now answers `Value::str("*")`.

The one consumer that reads `.parts` in the tree, zef's `DependencySpecification` version matching,
pads the list and joins it back with `.` before building a new `Version`; a `Whatever` and `"*"`
both join as `*`, so matching is unchanged. `t/types/temporal/version-parts-plus.t` pinned the old
`(2021, 10, *)`; it now pins `(2021, 10, "*")`, and `t/types/temporal/version-parts-whatever-str.t`
covers the rest (a lone `*`, one in the middle, before the `+` suffix, `.whatever`, the padding idiom).
