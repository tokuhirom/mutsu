# An lvalue subscript chain rooted at a subscriptable OBJECT is not routed through AT-KEY/AT-POS

**Designed: [ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md)**
(2026-09-05), Slices 4 (variable-rooted) and 5 (method-rooted). The companion
finding `todo/deep/native-method-cannot-return-an-lvalue-container.md` is Slices
2-3 of the same design. Read the ADR for the mechanism; this file records the
repro set.

When an lvalue subscript chain steps through an **object** implementing
`AT-KEY`/`AT-POS` — rather than a plain Array/Hash — the write does not reach
it.

## Repro set (re-measured 2026-09-05 against raku v2026.07 and `main` @ 37dd63f33)

All rows use `class Q { has %.d; method AT-KEY($k) is rw { %!d{$k} } }`.

| Spelling | raku | mutsu |
|---|---|---|
| `$u.query<foo>[0] = 99` (method-rooted, depth 2 — the original headline) | `{foo => [99 2]}` | loud refusal: `Cannot subscript-assign through %!query: it returned Q, ...` |
| `my $q = Q.new(...); $q<foo>[0] = 99` (**variable**-rooted, depth 2) | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent, exit 0** |
| `$u.query<foo> = 99` (method-rooted, depth 1) | `{foo => 99}` | `{foo => 1}` — **silent** |
| `my $t := $u.query; $t<foo>[0] = 99` | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent** |
| `$q.AT-KEY("foo")[0] = 99` (explicit spelling) | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent** |
| `$q<foo><bar>[0] = 99` (depth 3) | `{foo => {bar => [99 2]}}` | `No such method 'd' for invocant of type 'Hash'` — **the instance is replaced by a Hash** |
| `$q<foo> = 99` (var-rooted, depth 1) | `{foo => 99}` | correct |
| `my $e := $q<foo>; $e[0] = 99` | `{foo => [99 2]}` | **correct** |
| `$q<foo>.push(99)` | `{foo => [1 2 99]}` | correct |

**Correction to the previous text.** This file said the failure is "at least
loud and honest" now. That is true for exactly one of the six broken spellings;
the other four are silent, and the depth-3 one corrupts the object. The
variable-rooted silent row is the better acceptance case.

## Root cause (gdb-confirmed 2026-09-05)

`exec_index_assign_expr_nested_op` (`src/vm/vm_var_assign_index_named.rs:2963`)
*does* have an Instance branch, and it *is* entered. Breaking at :2984, :3014
and :3033 on the variable-rooted repro shows all three hit: the branch calls
`AT-KEY`, discards its container on the very next line —

```rust
let inner = self.call_method_with_values(target, at, vec![inner_idx.clone()])?
    .deref_container();
```

— writes only if the element is a `Proxy` or the inner value is an Instance with
`ASSIGN-*`, and otherwise falls through to the generic Hash/Array walk (:3033)
against a root that is not a container. That walk drops the write, and at depth
3 autovivifies a Hash over the instance.

The accessor is called as an **rvalue**. The `:=`-bound row above proves the
container-mode read of the same subscript already produces the right thing, so
the fix is to call it in lvalue mode and descend into what comes back — the walk
already descends `ContainerRef` (`descend_container_ref`, :3401).

The method-rooted half has the same shape one level up: the compiler temp
`bind_method_rooted_chain_root` installs
(`src/compiler/expr_closure.rs:606,639`) is filled by a plain
`self.compile_expr(cur)`, an rvalue read.

## Not the fix

Do NOT reintroduce an accessor-keyed slow path. The deleted
`__mutsu_index_assign_method_lvalue_nested` is exactly what dropped the writes
this ticket's neighbours were about
(`news/2026-09/method-rooted-lvalue-subscript-chain-writes-through.md`); its
copy-on-write rebuild is what made autovivified levels evaporate. ADR-0067's
routing adds no new walker.
