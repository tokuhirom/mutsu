# Parenthesized `has` attribute lists accept a per-attribute default

`has Numeric ($.tf-x = 0.0, $.tf-y = 0.0) is rw;` — the parenthesized multi-attribute form of
`has` — failed to parse when any listed attribute carried a `= default` initializer. mutsu's list
parser only accepted a bare sigil/twigil/name for each entry, so the `=` was left dangling and the
whole declaration fell through to "Undeclared routine: has used". This was the next load blocker
for `PDF::Content::Ops` and its dependents (`PDF::Font::Loader`, `PDF::Lite`, ...).

Rakudo parses the in-list default but ignores it: an attribute declared this way still reads back
as its declared type's own type object, exactly as an untouched `has Numeric $.x` would. mutsu now
matches that — it parses and discards the per-attribute `= EXPR`, and applies the same
auto-default (type object, or zero/empty for a native type, or the target type for a coercion
type) that a single-attribute `has` declaration already gets. `is rw` and the other list-level
traits continue to apply to every attribute in the list.
