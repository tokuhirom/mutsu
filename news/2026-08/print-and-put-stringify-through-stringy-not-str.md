# `print` and `put` use `.Str` for type objects

`render_str_value`, the shared coercion used by `print` and `put`, previously
gave a type object's user `.Stringy` method priority over `.Str`. That made
`print WithStringy` render `Stringy`'s result, while Rakudo uses `.Str` for
these output routines and instead warns and renders the empty string when no
user `.Str` exists.

The type-object branch now dispatches only a user `.Str`; otherwise it follows
the existing uninitialized-value warning path. The regression coverage checks
both a Stringy-only class and a class defining both methods, while retaining
prefix `~`'s separate Stringy behavior.
