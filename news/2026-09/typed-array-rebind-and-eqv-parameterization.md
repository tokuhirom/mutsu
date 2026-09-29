# Typed container rebinds check the declaration; `eqv` compares parameterization

Binding an `@`/`%` variable with `:=` copied the bound container's element type
into the variable's by-name constraint so element stores would obey it — but
the next `:=` then checked against that copy as if it were the declaration.
An untyped `my @a := Array[Int].new(...)` could never be rebound to an
`Array[Str]` ("expected Positional[Int] but got Array"), and a
`my Cool @c` bound to `Array[Int]` refused every later non-`Int` array.

The by-name type lane (`__mutsu_type::<name>`) now keeps both halves after a
bind that changes it: `Pair(declared => current)`. Element operations read the
current half; the bind checks read the declared one
(`var_declared_type_constraint`). Because both live in the one env entry,
block-exit restore, closure capture and re-declaration carry or drop the
declaration with no extra bookkeeping. A rebind reached by name — a closure or
nested sub rebinding a captured `@a`/`%h` — now goes through the same check and
propagation instead of writing into the old container, and binding to an untyped
container drops an earlier bind's element type.

`eqv` is type-strict, and now also compares the container's type
parameterization: `Array[Int].new(1,2,3) eqv [1,2,3]`,
`(my Int @d = 1,2,3) eqv [1,2,3]`, `Hash[Int].new eqv {}` and nested typed
arrays are `False`, as in Rakudo. The recursive `Value::eqv` and the VM's
lock-step array walk share one helper (`value::eqv_container_type`).

The stricter `eqv` exposed producers that dropped the parameterization: the
operator hyper and the code-ref hyper (`@a >>[&op]<< @b`) now share one
result-container rule (`hyper_list_result`) that returns the shape side's
`Array[T]` when every result fits and a `List` otherwise (rakudo#5778);
`deepmap` keeps the source's `Array[T]` likewise; and `Match.new(:hash(...))`
no longer copies the argument's `Map` tag into its internal named-capture store.
An expression-position assignment into an untyped `@`/`%` (`True and @h =
@typed`) now drops the source's type tag exactly like the statement form, which
leaked `Array[Str]` into Text::CSV's `kh => my @kh` header array.

Closes #9852.
