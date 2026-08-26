# A sigilless single-argument-rule slurpy (`+name`) binds a List, not an Array

`sub zipi(+zape) { zape.^name }` answered `Array` for every argument shape.
Rakudo answers `List` — and passes a lone lazy `Seq` straight through as a
`Seq`.

## The rule, as measured against raku v2026.06

For a **sigilless** `+a`:

| call | raku |
|---|---|
| `z("Hey")` | `List` (1 element) |
| `z(1, 2)` | `List` |
| `z()` | `List` (empty) |
| `z((1, 2))` | `List` |
| `z(@a)` (an `Array`) | `List` |
| `z(1 ... *)` | `Seq`, still lazy |

For a **sigiled** `+@a` the container declaration wins and the result is an
`Array` in every one of those cases except a lone lazy source, which is exposed
as a (still lazy) `List`. `*@a` binds an `Array` throughout, including for a
lazy source.

## Root cause

`Interpreter::bind_signature_params`'s `pd.onearg` branch
(`src/runtime/types/binding_signature.rs`) collected the arguments correctly but
finished with an unconditional `Value::real_array(items)` — `ArrayKind::Array` —
for both the sigilless and the sigiled form. A sigilless parameter is not a
container declaration, so it now takes `Value::array(items)` (`ArrayKind::List`).

The lazy case was a separate hole: the branch only recognized `ValueView::Seq`,
so `1 ... *` (a `ValueView::LazyList`) missed every list-shaped arm and fell
through to "wrap this one non-iterable argument", producing a one-element list
containing the whole sequence. Lone genuinely-lazy arguments are now bound
without reification — as-is for `+a`, and `with_list_context()`-tagged for
`+@a`, which keeps the generator lazy while making `.WHAT` report `List`.

Pinned by `t/signature-binding-gaps.t`.
