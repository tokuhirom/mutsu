# `.^can('bool')` no longer answers yes for every value

Raku spells the native integer types as coercion methods on `Cool`: `300.int8`
is `44`, `"300".byte` is `44`. mutsu implemented that by treating *any* native
integer type name as a 0-arg method on *any* value — the check was
`is_native_int_type(method)`, over the same list the type system uses.

Two things were wrong with reusing that list. It contains the C-width aliases
`NativeCall::Types` exports (`long`, `ulong`, `size_t`, `bool`, ...) and
`atomicint`, none of which Rakudo declares a method for — `42.bool` is
"No such method 'bool' for invocant of type 'Int'. Did you mean any of these:
'Bool'?". And the methods live on `Cool`, so a value that is not Cool has none
of them; `Pair` is the case that mattered.

Reporting a method that does not exist is not a harmless extra. `.^can` probes
native dispatch, so `$anything.^can('bool')` answered yes, and the shape

```raku
sub visit($context, Str:D $field) {
    if $context{$field}.defined { $context{$field} }
    elsif $context.^can($field) { $context."$field"() }
}
```

— probe, then fall through to the next candidate — stopped falling through.
`Template::Mustache`'s context lookup is exactly that shape, walking a stack of
`Pair` frames until one has the field. Asked for `bool` on a frame that did not
have it, `visit` returned `0` (the coercion's parse-failure default) instead of
`Nil`; `0` is `.defined`, so the caller took it and stopped walking, and the
section rendered as false.

The bug surfaced as **nondeterminism**, which is why it took a while to place:
the frame order comes from hash iteration, so whether the frame carrying the
field came first decided the outcome. `92-specs-file.rakutest` failed roughly
half its runs, and identical module trees at two paths appeared to disagree.

The coercion-method names are now their own list — `int8`..`uint64`, `byte`,
`int`, `uint`, the ones Rakudo actually declares — and the arm is gated on the
invocant being `Cool`. `42.int8` and `"42".byte` still work, `42.^can('bool')`
and `(a => 1).^can('int8')` are both empty, and Template::Mustache's
`92-specs-file` passes every run. Pinned by
`t/native-int-coerce-methods-are-cool-only.t`, whose 14 assertions also pass
unmodified under rakudo.
