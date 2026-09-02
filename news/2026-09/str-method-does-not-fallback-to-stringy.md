# `.Str` does not fall back to a user-defined `Stringy`

`Stringy` is a separate role from `Str`. The default `Stringy` implementation
delegates to `.Str`, but an explicit `.Str` call does not reverse that direction
and call a user-defined `.Stringy` when the class has no user `.Str` method.

mutsu used to apply that reverse fallback to user-defined classes. A class that
defined only `Stringy` therefore produced its custom string from `.Str`, while
Rakudo used the default `Mu.Str` object representation. Explicit `.Str`
consumers now keep the same semantics: list stringification, `join`, and
`sprintf("%s", ...)` call `.Str` without probing `Stringy` first.

String context remains intentionally `Stringy`-first, so prefix `~` and string
interpolation continue to use a user-defined `Stringy` method. Role-mixed values
follow the same distinction.

Pin: `t/str-method-falls-back-to-stringy.t`.
