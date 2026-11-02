# A method call no longer re-walks the MRO for a built-in of the same name

`build_method_dispatch_frame` asked, on every call of a method the user
overrides, whether the class's MRO reaches a built-in method of that name
(`builtin_method_level`): a scan of every MRO level against the method table
(about 320 `owner_has_row` probes per AttrX::Mooish iteration). The answer
depends only on the MRO and the fixed table, so it is memoized per
`(class, method)` for one registry generation and cleared with the other
method caches.

The bound-decont marker's env key is likewise built from the local's interned
name instead of re-interning its text on every scalar assignment.

Part of #15688.
