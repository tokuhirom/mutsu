# Apply variables pragma to untyped declarations

An active `use variables :D` or `:U` now gives untyped lexical scalar, array,
and hash declarations an implicit `Any:D` or `Any:U` constraint. The constraint
is registered for later assignments as well as initialization, including when
the declaration is hoisted. Untyped `&` and sigilless bindings keep their
existing behavior.
