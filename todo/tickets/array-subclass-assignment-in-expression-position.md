# An `is Array` subclass crossing an assignment: the two paths disagree in both directions

Split out of `news/2026-09/array-subclass-iterator-override.md` (2026-09-07),
which fixed the statement-position spelling. Re-measured and re-scoped
2026-09-07 after an attempt that produced a working half and a blocked half —
see "What an attempt established" below, which is the part worth reading before
touching this again.

## Repro

```raku
class SA is Array { }

# (1) a `@`-bound instance: list assignment must DISTRIBUTE its elements
my @a := SA.new(3,2,1,4);
my @c = @a;              say @c.raku;   # raku [3, 2, 1, 4]   mutsu [3, 2, 1, 4]   OK
say (my @b = @a).raku;                  # raku [3, 2, 1, 4]   mutsu [[3, 2, 1, 4]]
my @d; say (@d = @a).raku;              # raku [3, 2, 1, 4]   mutsu [[3, 2, 1, 4]]

# (2) a `$`-held instance: list assignment must store it as ONE element
my $e = SA.new(3,2,1,4);
my @h; @h = $e;          say @h.raku;   # raku [[3, 2, 1, 4],]   mutsu [3, 2, 1, 4]
say (my @f = $e).raku;                  # raku [[3, 2, 1, 4],]   mutsu [[3, 2, 1, 4]]   OK
```

So this is **not** "value position is missing the rule". The two directions are
wrong in opposite ways, and each is right in exactly the position where the
other is wrong. An `iterator` override behaves the same way in direction (1)
(`class SB is Array { method iterator() {…} }`, value position gives
`[[7, 8, 9]]` where rakudo gives `[7, 8, 9]`).

## What an attempt established (2026-09-07)

**Direction (1) is a solved, mechanical fix, and it is small.** There are
exactly **three** `@`-store routes, and only one of them carries the rule:

| route | opcode | where | has the rule |
|---|---|---|---|
| statement `my @c = @a` | `SetLocalDecl` | `src/vm/vm_var_assign_set_local.rs` | yes |
| expression `(my @b = @a)` | `SetGlobal` | `src/vm/vm_exec_dispatch.rs`, the `name.starts_with('@')` arm | no |
| expression `(@d = @a)` | `AssignExprLocal` | `src/vm/vm_var_assign_local.rs`, the `name.starts_with('@')` arm | no |

(`--dump-bytecode` shows the split directly; the expression-position declaration
compiles through `SetGlobal` because `decl_slot` is `None` there.) Lifting the
`try_iterable_instance_items` / `__mutsu_array_storage` chain out of
`set_local` into one helper — `positional_assign_instance_items`, next to
`try_iterable_instance_items` in `src/vm/vm_for_loop_dispatch.rs` — and calling
it from all three fixes every row of direction (1), including the `iterator`
override. That was built and measured.

**Direction (2) is the blocker, and it is a representation question, not a
routing one.** The reason no store route can tell the two directions apart is
that mutsu does not itemize an `is Array` subclass instance when it is stored in
a `$`:

```
my $e = SA.new(3,2,1,4);  say $e.raku;   # raku $[3, 2, 1, 4]   mutsu [3, 2, 1, 4]
```

An ordinary `my $x = [1,2,3]` *is* itemized (mutsu's `@y = $x` correctly gives
one element), so the missing itemization is what leaves the instance
indistinguishable from the `@`-bound one at every `@`-store site. Fixing
direction (1) alone therefore **regresses** the two direction-(2) rows that were
accidentally right, which is why the attempt was withdrawn whole rather than
half-landed.

The obvious itemization — a `Scalar` wrapper in
`Interpreter::itemize_scalar_store_value`, which is exactly what the `Range` arm
right below it does, since an instance has no itemized representation of its own
— was tried and **measured to break the receiver**:

```
class SA is Array {}; my $c = SA.new(1,2);
$c.elems      # 1   (raku 2)      -- the wrapper's own element count
$c.join("-")  # SA()  (raku 1-2)
for $c { … }  # $_ gists as SA()  (raku "1 2")
```

Adding the unwrap to `call_method_with_values_inner` was not enough: the hot
method-call paths reach `delegates_to_array_storage` from **three** places
(`src/vm/vm_call_method_ops.rs`, `src/vm/vm_call_method_mut_ops.rs`,
`src/runtime/methods_call_dispatch.rs`), each testing `target.view()` for
`ValueView::Instance` directly, and `for`-loop iteration and stringification
test it again. A `Scalar`-wrapped instance is invisible to all of them.

## The decision this needs

How is "this container-subclass instance is held in a `$`" represented, such
that every existing `ValueView::Instance` test still sees an instance?

Three candidates, none free:

1. **`Scalar` wrapper + make the wrapper transparent** at every method-call
   receiver, `for`-loop source, and stringifier. Semantically the cleanest (it
   is what `Range` does, and what the ADR-0064 container-transparency rule
   already says), and the largest diff. Ordering matters: `.VAR`, `.raku` and
   `.perl` must keep seeing the wrapper, or `$e.raku` loses its `$`.
2. **Itemize the backing `__mutsu_array_storage`** to an itemized `ArrayKind`.
   Contained, but `value_to_list` on an itemized array yields one element, so
   every delegation through the storage (`.elems`, `.join`, …) breaks the same
   way — measured to be the same dead end as (1) without its upside.
3. **A marker attribute** on the instance, consulted only by
   `positional_assign_instance_items`. Smallest possible diff and nothing else
   changes — but it stores a *container* property inside the *object*, and
   instance attributes are deep-copied on assignment, so `my @z := $e` would
   inherit an itemization that is not its own. A band-aid; would need a
   `// TODO:` and probably does not survive review.

(1) looks right and is plausibly ADR-sized. Do not start direction (1)'s routing
fix without settling this first — on its own it trades three wrong rows for two.

## Check when fixing

`say (my @b = @a).raku` and `say (@b = @a).raku` for an already-declared `@b`;
the same two with an `iterator` override (which must follow the override, as the
statement form does); `my $c = SA.new(...)` in **both** positions, which must
give one element in both; `$c.elems` / `$c.join` / `for $c` / `$c.raku` /
`$c.push`, which is where the itemization representation shows; and
`t/array-subclass-iterator-override.t`, which pins the statement-position
behaviour and does **not** currently pin the `@e = $c` row — add it.
