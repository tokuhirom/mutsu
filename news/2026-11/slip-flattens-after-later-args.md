# `|@a` flattens after the arguments that follow it

A slip in an argument list or a list literal now flattens when the capture or
list is built, after every argument has been evaluated, as rakudo does
(#19135). `f(|@p, :t(@p.pop))` with `@p = 1, 2, 3` binds `[1 2]` and
`:t(3)`, and `(|@p, @p.shift)` is `(2 3 1)`. mutsu copied the elements where
the slip was written, before the later arguments ran, so it passed `[1 2 3]`.

TOML's date parser relies on it: `DateTime.new(|@part, :timezone(@part.pop))`
pops the timezone off the parts it slips. Once `DateTime.new` refused a
seventh positional argument (#18953), as rakudo does, TOML's `t/04-nqp.t`
failed with "Cannot resolve caller new(DateTime:U: Int:D x7, :timezone(Int))".

The compiler now leaves a slip with a later argument that may have an effect
unflattened on the argument stack (the container itself, which shares its
storage with the variable) and flattens it in place with the new `MakeSlipAt`
op once the last argument is pushed. This covers sub, method and code-variable
calls and list and array literals. A slip followed only by variables and
literals compiles as before.
