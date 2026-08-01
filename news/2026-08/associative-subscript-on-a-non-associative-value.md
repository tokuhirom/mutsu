# An associative subscript of a non-Associative value

The value adverbs already read a *positional* subscript of a non-`Positional`
value through the one-element-list rule
([subscript-kind-on-the-exists-opcode](../2026-07/subscript-kind-on-the-exists-opcode.md)).
The mirror case — an *associative* subscript of a value that does not do
`Associative` — answered `Nil` to every spelling. All eight now match raku:

| expression | raku | was | now |
| --- | --- | --- | --- |
| `5<a>:v` | `()` | `Nil` | `()` |
| `5<a>:k` | `()` | `Nil` | `()` |
| `5<a>:kv` | `()` | `Nil` | `()` |
| `5<a>:p` | `()` | `Nil` | `()` |
| `5<a>:!k` | `"a"` | `Nil` | `"a"` |
| `5<a>:!v` | `Failure(X::AdHoc)` | `Nil` | `Failure(X::AdHoc)` |
| `5<a>:!kv` | `("a", Failure)` | `Nil` | `("a", Failure)` |
| `5<a>:!p` | `:a(Failure)` | `Nil` | `:a(Failure)` |

raku's model is one rule applied twice: `Any.EXISTS-KEY` is always `False`, so
every key is missing — a `keep_missing = False` adverb finds nothing and yields
the empty list, while a negated one must produce a value and therefore calls
`Any.AT-KEY`, which fails with `X::AdHoc` ("Type Int does not support
associative indexing."). The `:!kv` / `:!p` rows are that same exception arriving
in the value position as a `Failure` rather than being thrown.

`builtin_subscript_adverb` already knew the bracket, so the fix is a branch that
recognises the shape up front and fills the rows with `(key, Any.AT-KEY, False)`.
An `Array` target counts as non-`Associative` — it does `Positional` — which also
repairs `@a<a>:!k`, previously the `-1` of a numified positional lookup rather
than the key `"a"`. A **type object** is the one exception: an undefined
invocant answers `Any` instead of failing, so `Int<a>:!v` is `Any` where
`5<a>:!v` is the Failure.

Three things came along with it, all in the same protocol:

- **`Pair` subscript adverbs.** A `Pair` does `Associative` with exactly one
  entry, so `(:x(1))<x>:v` is `1` and `(:x(1))<y>:v` is `()`; a missing key on a
  Pair reports the plain `Nil` of an absent entry, not the `Any.AT-KEY` failure a
  non-`Associative` gets. These answered `Nil` across the board too.
- **`.AT-KEY` / `.EXISTS-KEY` as methods.** `5.AT-KEY("a")` and
  `5.EXISTS-KEY("a")` died with `X::Method::NotFound`; they now answer the same
  Failure and `False` the subscript path uses, and `Pair` gained its own arms. An
  `Instance`/`Mixin`/`Package` target still falls through to the runtime
  dispatcher, so a user-defined `AT-KEY` is not shadowed.
- **The positional twin.** `5[1]:!v` was `Nil` where raku reports a per-index
  `Failure(X::OutOfRange, got => 1, range => "0..0")`: the scalar coerced to a
  one-element list reported a missing slot with the flat container default
  computed for Range/Seq coercions. `PositionalMissing` now distinguishes "the
  container's element default" from "a scalar read past its one slot", which is
  the only case that needs the index to build its answer.

While pinning the last point, the `List` default turned out to be wrong in the
same expression: `(1, 2)[5]:!v` reported the `Any` hole of a real `@`-array where
raku reports `Nil`. A `List` has no element type, so only a real array (which
`$[1,2]` is and `$(1,2)` is not) reports `Any`.

Still open, and not reachable from here: `5<a>:delete` answers `Nil` where raku
fails with "Can not remove values from a Int". The delete opcode carries no
bracket-kind marker, so it cannot tell `5<a>:delete` from `5[1]:delete` — that
needs the subscript-kind marker plumbed through the delete op first.

Pinned in `t/subscript-kind-associative.t`, the associative mirror of
`t/subscript-kind-positional.t`; all 46 assertions pass unmodified under rakudo.
