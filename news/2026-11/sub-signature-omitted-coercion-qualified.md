# Omitted coercion-typed leaf of a sub-signature binds the qualified type object

`method COERCE(Map (Column::List(Any) :$pb))` bound the unqualified `Column::List` when `:pb` was
omitted, so a `Column::List $.pb` attribute rejected it (SQL::Abstract's `Window::Definition`,
#16752). The sub-signature binder now uses the same omitted-optional value as a top-level
parameter.
