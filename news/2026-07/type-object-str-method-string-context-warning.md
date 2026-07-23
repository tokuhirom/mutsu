# `.Str` / `.Stringy` on a bare type object warns in string context

An explicit `Int.Str` / `Str.Stringy` call on a bare type object stringified to
the empty string *silently*, where raku emits the `Use of uninitialized value of
type X in string context.` warning. Prefix `~`, interpolation, `print`, and hash
keys already warned; the explicit method form was the remaining silent path.

```raku
Int.Str       # "" + warning (was: "" silently)
Str.Stringy   # "" + warning
role R { }; R.Str   # "" + warning
```

## Implementation

The bare type-object arm of the slow-path `Str`/`Stringy` handler
(`methods_instance_ops.rs`) returned `Value::str("")` directly. It now routes
through `Interpreter::warn_type_object_string_context`, the shared helper that
raises the warning and resumes with `""`.

`.Stringy`'s default implementation is `self.Str`, so a type object whose class
defines a user `.Str` (but no `.Stringy`) stringifies through it — the handler
delegates `.Stringy` to the user `.Str` before warning. `.Str` never falls back
to `.Stringy`, matching raku. A user `.Str`/`.Stringy` is dispatched before
reaching this fallback, so it never triggers the warning.

`.gist` / `.raku` are unchanged (they render `(Int)` / `Int` without warning).

Pin: `t/type-object-str-method-warning.t`.
