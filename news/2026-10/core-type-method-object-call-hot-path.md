# Core-type Method-object dispatch no longer scans builtin rows on every call

The core-type Method-object branch added to `call_sub_value` in #12171 ran
`is_builtin_type_method` (which collects every builtin method row of a type and its
ancestors) for every sub-value call whose package was not a registry class. That made
`roast/S32-str/Collation.t` go from 2 s to over 2 minutes and time out in CI. The branch now
asks the O(1) builtin-type catalog first, so only a package that is a core type pays for the
row scan. The behaviour pinned by `t/oo/method/core-type-method-object-call.t` is unchanged;
the performance regression is pinned by the whitelisted `Collation.t` itself.
