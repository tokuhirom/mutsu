# Two-level autovivification through an undefined scalar dropped the write

```raku
my $beatles;
$beatles{"White Album"}[0] = "Back in the U.S.S.R.";
say $beatles.raku;
# raku:  ${"White Album" => $["Back in the U.S.S.R."]}
# mutsu: Any
```

Found by the doc-diff harness on `Language/subscripts.rakudoc:418`.

## Root cause

Not "chained autovivification is unimplemented" — the depth mattered. Probing
each shape isolated it precisely:

| chain | mutsu (before) | raku |
|---|---|---|
| `$v<a> = 1` | `{:a(1)}` | `${:a(1)}` |
| `$v<a><b> = 1` | **`Any`** | `${:a(${:b(1)})}` |
| `$v<a>[0] = 1` | **`Any`** | `${:a($[1])}` |
| `$v[0]<b> = 1` | **`Any`** | `$[{:b(1)},]` |
| `$v[0][1] = 1` | **`Any`** | `$[[Any, 1],]` |
| `$v<a><b><c> = 1` | `{:a(${:b(${:c(1)})})}` | `${:a(${:b(${:c(1)})})}` |

Exactly the two-level chains failed. Those compile to `IndexAssignExprNested`;
one level compiles to `IndexAssignExprNamed` and three or more to
`IndexAssignDeepNested`. `exec_index_assign_expr_nested_op`
(`src/vm/vm_var_assign_index_named.rs`) autovivified its root only when the
variable was *absent from the env* — but `my $beatles;` declares it holding the
`Any` type object, so the check passed, no container was created, and the write
went into a throwaway temporary. The deeper op survived the same weak check only
because its pointer walk has a "retry this level" fallback that clobbers a
non-container root with a fresh container.

## Fix

The root is now vivified when the variable is absent **or** holds an undefined
value (`Nil` or a type object — the same test `.defined` uses), which is what
Raku's autovivification actually means. A *defined* value is deliberately left
alone: raku dies there with "Type Int does not support associative indexing",
so silently clobbering it would be wrong.

The freshly vivified container is additionally itemized for a `$`-sigil root
(`itemize_for_element_store`), because a container held by a Scalar container
renders as `${...}` / `$[...]` — the same rule already applied to a nested
autovivified *element*. With that, all four two-level shapes above match raku
character for character, including the ticket's own example.

An rvalue *read* still creates nothing: `$h<zz>`, `$h<a><zz>` and even
`$h<q><r>` leave `:exists` false and `.elems` unchanged at every level.

Pinned by `t/lexical-decl-and-autoviv.t`, which asserts `:exists` and `.elems`
on untouched sibling paths rather than only the stored value.

## Residual, not fixed here

The itemization gap is still visible at the other depths — `my $v; $v<a> = 1`
renders `{:a(1)}` and the three-level chain renders `{:a(...)}`, both missing
raku's leading `$`. That is a wider, pre-existing divergence in how a container
stored into a `$` scalar is itemized (it shows up for plain `my $k = %x` too,
which involves no autovivification at all), and belongs to that family rather
than to this one.
