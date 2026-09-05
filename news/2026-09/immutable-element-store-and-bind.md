# An immutable container has no assignable element, and a `$`-bind of one has no container

Five rows of the "immutable lvalues mutsu still lets you assign to" survey
answered `OK` where rakudo throws. They turned out to be three separate gaps in
two mechanisms — the `:=` declaration bind and the element store — plus one
purely cosmetic message difference. All five now match rakudo exactly:

| | rakudo | mutsu before | mutsu now |
|---|---|---|---|
| `my $x := (5, 6)[0]; $x = 10` | `X::AdHoc` "Cannot assign to an immutable value" | silently succeeds | matches |
| `my $x := (1, 2, 3); $x = 5` | same | silently succeeds | matches |
| `my $s = (1,2,3).Seq; $s[0] = 5` | `X::Assignment::RO` "Cannot modify an immutable Int (1)" | silently succeeds | matches |
| `(1..3)[0] = 9` | "Cannot modify an immutable **Range** (**1..3**)" | "... immutable **value** (**1 2 3**)" | matches |
| `(1,2,3)[0] = 9` | "Cannot modify an immutable List (**(1 2 3)**)" | "... List (**1 2 3**)" | matches |

## A declaration bind settles its writability from the source — for `$` too

`news/2026-09/sigilless-bind-writability-comes-from-its-source.md` established
that rule for `my \x := ...`. The `$`-sigil spelling had no equivalent verdict
step, so it inherited whatever container the promotion primitive happened to
mint, and two shapes came out wrong.

**The element shape.** `OpCode::IndexAutovivifyLazyTerminal` promotes any scalar
leaf to a fresh `ContainerRef` cell, immutable `List` included, so the write
landed in a cell only the binding could see. The sigilless spelling already
suppressed that promotion through a narrow `sigilless` flag. That flag was
always about a *declaration* bind rather than about the sigil — a sigilless term
and a `$` scalar both take their writability from what the RHS denotes — so it
is now `decl_bind`, set for `my $x := <subscript>` as well. `SetLocal` then sees
a plain `Int` and marks the name immutable through the path `my $x := 5` already
took.

**What has to select that flag, and what does not.** The obvious selector is
"this is a `:=` VarDecl with a scalar-shaped name", and it is wrong: a
multi-parameter loop chunk is desugared by `build_for_bind_stmts` into the very
same `SyntheticBlock([MarkBind, VarDecl])` over a synthetic subscript of the
per-chunk topic, and it
*depends* on the promotion to refresh its parameter each iteration. Selecting on
that shape re-broke exactly the two consumers this ticket had recorded — a
chunked `for ($p, $q, $r) -> \x, \y, \z` write-through and `for $b.kv -> \k, \v`
on a `BagHash` — caught by `t/for-list-multi-param-write-through.t` and
`t/for-quanthash-values-rw-writeback.t`. The selector that actually separates
them is the parser's own `__scalar_bind` trait, which only a declaration written
as `my $x := ...` / `my \x := ...` in source carries; the loop desugar builds
its `VarDecl` with no custom traits at all.

One thing had to be widened for that verdict to be reachable at all:
`compile_call_arg` wraps a bind-to-subscript in a synthetic
`__mutsu_bind_index_ref_N` `WrapVarRef`, and `SetLocal` was reading any wrapper
as "this bind has a named source, leave it writable". That wrapper denotes
nothing of its own, so the value under it is the oracle — exactly what
`MarkSigillessBindSource` already says for the sigilless twin.

**The whole-container shape.** `SetLocal`'s `:=`-to-literal marking fired only
for pure immutable *scalars* (`Int`/`Str`/`Num`/`Rat`/`Bool`/`Complex`), on the
stated grounds that anything container-like must stay writable. But an immutable
Positional is a container that is not a **Scalar** container, and rakudo's
scalar assignment needs one: `my $x := (1, 2, 3); $x = 5` dies exactly like
`my $x := 5` does. The allowlist now also covers a `List`/`ItemList` literal, a
`Range`, a `Seq`, a lazy list and a `Slip`. It is still deliberately an
allowlist — `ContainerRef`, `Proxy`, deferred `HashEntryRef` binds, real
`Array`/`Hash` values and instances stay writable, and marking less is the safe
direction.

## A `Seq` element store is decided per element

`try_seq_element_cell_assign` already existed for the element-producing `Seq`
shapes (`@a.values`, `@a.kv`, `@a.pairs`), whose elements are the producer's
live cells and are written through. It returned `None` for every other `Seq`,
and the write was then silently dropped. rakudo decides this per ELEMENT rather
than per sequence, so the helper now covers both halves of that one rule: a
container element is written through (`my $x = 1; my $s = ($x, 2).Seq;
$s[0] = 5` sets `$x` to 5, verified against rakudo), and a plain element is
refused naming itself ("Cannot modify an immutable Int (1)"), with a
past-the-end element refused as rakudo's "Cannot modify an immutable Nil
value". A body that still has a producer to pull from is left alone: forcing it
merely to refuse the store would consume a one-shot iterator and could run
forever on an unbounded one.

## The refusal message names the type and gists the value

`RuntimeError::assignment_ro_value` rendered "Cannot modify an immutable value
(`.Str`)". rakudo names the value's TYPE and renders its `.gist`, which is why a
`List` prints one paren pair more than mutsu did and a `Range` prints `1..3`
rather than its stringified elements. The constructor now does that, and the two
hand-rolled copies of the same message in the named and generic element-store
paths were folded into it.

## Coverage

`t/immutable-element-store-and-bind.t` — 27 assertions, every one dual-oracled
against rakudo: the two bind shapes and their still-writable neighbours (an
`Array` element, a container inside a `List` literal, a deferred past-the-end
element, a whole-`Array` bind), the `Seq` store in both refusing and
writing-through flavours, and each message shape in both its anonymous and named
spellings.

`t/producer-seq-named-receiver-write.t` lost a `todo`: its "a non-producer Seq
subscript assignment is accepted (pre-existing)" row now throws exactly what
rakudo throws, so it is a plain `throws-like` asserting the element-named
message.

## What is left, and it is not this mechanism

Two neighbours measured while here still diverge and stay recorded in
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`: a `gather`
sequence's element store (`my $s = (gather { take 1 }); $s[0] = 5`), which is a
`LazyList` in mutsu and shares its representation with the lazy `@`-array whose
element assignment is *legitimate*; and an associative subscript of a `Seq`
(`$s<a> = 5`), where rakudo refuses the subscript itself rather than the store.
