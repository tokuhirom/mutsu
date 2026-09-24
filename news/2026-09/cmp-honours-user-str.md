# `cmp` and the default sort order honour a user `Str`

Rakudo's `infix:<cmp>(\a, \b)` falls back to comparing the operands' `.Stringy`
once no `Real` or structural candidate applies. mutsu's `cmp_value` instead
compared an object operand through the pure stringifier (its `.gist`), so a
class with its own `method Str` was ordered by the wrong text:

```raku
class S { method Str { "b" } }
say S.new cmp "a";              # rakudo: More   (mutsu said Less)
say (S.new, "a").sort.map(~*);  # rakudo: (a b)  (mutsu said (b a))
```

`leg`, `eq` and `lt` had already been routed through
`coerce_str_compare_operands` in the ADR-0118 §2.6 work (#9230); `cmp` was left
out because it carries its own structural candidates. It now takes the same
coercion when either operand is a concrete non-`Real` object whose class defines
`Str` or `Stringy`, after the Blob, NaN, Range, list and infinity candidates
have declined.

The default-order sort used the pure `compare_values` in four places that never
reached `cmp_value`: the pure `.sort` method fast path, the pure `sort()`
function fast path, the shared `sort_items_generic` / `sort_indices_generic`
orchestration, and the element-container producer behind `for @a.sort -> $v`.
The two pure fast paths now decline when an element is an object (new helper
`sort_needs_dispatched_cmp`), and the interpreter-side paths sort through the
dispatched `cmp` when an element has a user stringifier (`SortCaller` grew
`has_user_stringifier` / `dispatched_cmp`).

Pinned by new rows in `t/types/string/str-operator-forms-parity.t`. Closes #9232.
