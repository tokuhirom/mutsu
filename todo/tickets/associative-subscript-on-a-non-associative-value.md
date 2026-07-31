# `5<a>:v` answers `Nil` where raku answers `()`

The value adverbs now read a *positional* subscript of a non-`Positional` value
through the one-element-list rule
([news](../../news/2026-07/subscript-kind-on-the-exists-opcode.md)). The mirror
case — an *associative* subscript of a value that does not do `Associative` —
is still unhandled, and every form answers `Nil`:

| expression | raku | mutsu |
| --- | --- | --- |
| `(5<a>:v).raku` | `()` | `Nil` |
| `(5<a>:k).raku` | `()` | `Nil` |
| `(5<a>:kv).raku` | `()` | `Nil` |
| `(5<a>:p).raku` | `()` | `Nil` |
| `(5<a>:!k).raku` | `"a"` | `Nil` |
| `(5<a>:!v).raku` | dies: `Type Int does not support associative indexing.` | `Nil` |
| `(5<a>:!kv).raku` | `("a", Failure)` | `Nil` |
| `(5<a>:!p).raku` | `:a(Failure)` | `Nil` |

raku's model is consistent: `Any.EXISTS-KEY` is always `False`, so a
`keep_missing = False` adverb finds nothing and yields the empty list, while a
`keep_missing = True` adverb must produce a value and therefore calls
`Any.AT-KEY`, which dies with `X::AdHoc` ("Type Int does not support associative
indexing"). The `:!kv` / `:!p` rows show the same exception arriving as a
`Failure` in the value position rather than being thrown.

`builtin_subscript_adverb` (`src/runtime/builtins_multidim_subscript.rs`) knows
the bracket now — `subscript_is_positional` is `Some(false)` for these — so the
hook exists; what is missing is the `Any.AT-KEY`/`EXISTS-KEY` behaviour for a
non-`Associative` target. Coercing such a target to an empty Hash would get the
four `keep_missing = False` rows and `:!k` right in a few lines, but would answer
the remaining three with `Any` instead of the `Failure`, so it is worth doing
together with a real `Any.AT-KEY` that produces the typed exception.

The positional twin of the last point is also open: `5[1]:!p` is
`1 => Failure(X::OutOfRange)` in raku and `1 => Nil` in mutsu, because the
coerced one-element array reports a missing element with the flat `missing_value`
computed for Range/Seq coercions rather than a per-index out-of-range `Failure`.
