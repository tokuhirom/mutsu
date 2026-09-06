# `.VAR` on an itemized list element reports `Scalar` again

```raku
say (1, 2, 3, $(4, 5))[3].VAR.^name;
# raku: Scalar   mutsu (before): List
```

The `$(...)` itemization is exactly what puts a `Scalar` container around the
inner list, and `.VAR` is the one introspection that is supposed to report it —
which is the point of the doc block this came from
(`raku-doc/doc/Language/structures.rakudoc`, illustrating "itemization is what
makes a list one element"). mutsu answered for the inner `List` instead.

## Root cause

The compiler routes a `.VAR` on a subscript through
`__mutsu_anon_index_var_meta` → `Interpreter::element_var_meta`, which asks the
*parent* whether its element slots are containers. For a `List` parent they are
not — and correctly so: `(1, 2)[0].VAR.^name` is `Int` in raku, not `Scalar` —
so it handed the element straight back.

That is the right answer for a plain element and the wrong one for an itemized
one. Itemization is a property of the *element*, not of the parent's slots: even
where the parent stores plain values, `$(4, 5)` carries a `Scalar` container of
its own. `element_var_meta` now checks the element before taking that early
return.

The check needed a small shared helper, `runtime::utils::value_is_itemized_container`,
because itemization has three spellings in mutsu and only one of them is the
obvious `ValueView::Scalar` wrapper: `$(1, 2)` / `$[1, 2]` share their backing
storage with the plain form and carry the itemization in the `ArrayKind`, and
`$(%h)` carries it as a flag on the `Hash` repr. All three occur as list
elements, and all three now answer `Scalar`.

## What did not change

`.VAR` is not a sound test that a container arrived — it reports `Scalar`
whether or not one did — so the pin checks the itemization through `.raku`,
`.elems` and `.WHAT` too, and covers the rows that must stay as they are:

- a non-itemized element keeps its own type (`(1, 2)[0].VAR.^name` is `Int`,
  `((1, 2), (3, 4))[0].VAR.^name` is `List`);
- a slice hands back a `List` of elements and `.VAR` on a `List` is identity;
- a real `Array`/`Hash` element is a container regardless (ADR-0040 slices 1-2
  itemize everything they store), including a chained `%d<a><b>` / `@g[0][1]`;
- `.VAR.name` / `.VAR.^name` on a named container are untouched.

`t/var-on-itemized-list-element.t`, 29 assertions, all measured against raku
v2026.07 first.
