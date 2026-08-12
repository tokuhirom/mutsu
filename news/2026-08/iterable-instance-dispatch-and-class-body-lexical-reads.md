# Iterable-instance iteration methods and class-body lexical reads (Text::CSV 79_callbacks green)

Two general interpreter bugs found via Text::CSV's `t/79_callbacks.t`
(predefined filter hooks), which now passes 90/90 (was: aborting at 38).

## 1. `.first`/`.map`/... on a user Iterable instance ignored its iterator

A class that `does Iterable` and defines `method iterator` was treated as a
single opaque item by Any's iteration methods — `$row.first: { $_ ne "" }`
tested the row object itself (whose `Str` is the joined line, hence truthy
for any multi-field row), so Text::CSV's `not_empty`/`filled` predefined
filters passed all-empty rows.

Fix: `call_method_with_values` now routes exactly the methods Rakudo sends
through `self.iterator` — `first`/`map`/`grep`/`sort`/`head`/`tail`/`flat`
(measured; `join`/`reverse`/`list`/`elems`/`kv`/`pairs`/`values`/`Array`/
`cache`/`eager` deliberately keep single-item semantics to match Rakudo) —
through `try_iterable_instance_items` (the same driver `for $obj` uses),
only when the class does not provide the method itself. A matching bypass
in `try_native_method_raw` keeps the native `flat` impl from intercepting
first. Pin: `t/iterable-instance-list-methods.t`.

## 2. Class-body reads of a sibling statement's `my` hash/array got a fresh empty one

Each class-body statement compiles as its own chunk, so a read of a `my`
hash/array declared by an earlier body statement was compiled
package-qualified (`%C::predef`) while the declaration flushed to env under
the bare sigiled name (`%predef`). `GetHashVar`/`GetArrayVar` had no
qualifier-stripping fallback (scalars already had one in `GetGlobal`) and
silently produced a brand-new empty Hash/Array. Text::CSV's

```raku
%predef-hooks<not-empty> = %predef-hooks<not_empty>;
```

alias rows therefore assigned `Any`, and `callbacks("filter", "not-empty")`
died with error 1004.

Fix: `auto_qualified_bare_env_read` (mirrors the `GetGlobal` bare-component
fallback; only fires when the qualifier is exactly the current package, so
foreign `%Other::h` reads still cannot reach another package's `my`
lexicals). Pin: `t/class-body-lexical-read.t`.

Residue filed as `todo/tickets/class-body-scalar-reassignment-lost.md`: the
WRITE side of the same asymmetry (`$x = 20` in a class body lands under
`C::x`, which nothing reads back).

Text::CSV suite frontier after this: `55_combi` (regex-engine panic at
test 78), `66_formula` (type-check failure at line 129), `46_eol_si`
(typed-array `.raku` parameterization + a type-object-gist row).
