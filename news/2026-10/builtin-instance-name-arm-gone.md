# `.name` on a built-in instance with no `name` method is `X::Method::NotFound`

The instance dispatch kept a by-name arm that answered `.name` on every
instance of a built-in class from its `name` attribute, or `Nil` without one,
so `Date.today.name`, `IO::Path.new("x").name` and `Instant.from-posix(1).name`
quietly answered `Nil` where Rakudo raises `X::Method::NotFound` (#13630). The
classes Rakudo declares `name` on (`Thread`, `Attribute`, `Label`, the
encodings, the `Systemic` classes, ...) answer it from their method-table rows,
so the arm is deleted.
