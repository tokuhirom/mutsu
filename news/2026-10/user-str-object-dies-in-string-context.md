# A user `Str` that answers an object dies in a native-string context

`class Foo { method Str { self } }` (Red::AST does this) answers an object, not
a string. Rakudo unboxes it with `nqp::unbox_s` wherever it needs native text,
and dies `This type cannot unbox to a native string: P6opaque, Foo`; Red's
`ResultSeq.map` relies on that to fall back to the real rows. mutsu stringified
the object silently (`Foo<25>`). One primitive, `unbox_str_answer`, now does
the unboxing and the refusal for infix `~` and the string comparators,
interpolation, `join`, list/`Array` stringification and `print`/`put`; a lone
`"$f"` / `"{$f}"` still hands the object back untouched, as rakudo does.
`IO::Path` arguments (`.add($f)`) are not routed through it yet. Fixes #15117.
