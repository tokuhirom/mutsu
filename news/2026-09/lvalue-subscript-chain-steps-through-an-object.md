# An lvalue subscript chain now steps through an object

An lvalue subscript chain that passes through an object implementing
`AT-KEY`/`AT-POS` — rather than a plain `Array`/`Hash` — used to lose the write.
`class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }` followed by
`my $q = Q.new(d => {foo => [1,2]}); $q<foo>[0] = 99` left `{foo => [1 2]}` and
exited 0, where raku answers `{foo => [99 2]}`. At depth 3 it was worse than
silent: `$q<foo><bar>[0] = 99` replaced the object with a freshly autovivified
`Hash` and then died with `No such method 'd' for invocant of type 'Hash'`.

This is [ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md)
slice 4 — part 5 of that design, the variable-rooted half.

## Root cause

`exec_index_assign_expr_nested_op` did have an `Instance` branch, and it *was*
entered; `rust-gdb` breakpoints on the branch, on its Proxy probe, and on the
generic walk below it all fired on the repro. The branch called the accessor as
an **rvalue** and discarded its container on the very next line
(`call_method_with_values(...).deref_container()`), wrote only when the element
happened to be a `Proxy` or the inner value was itself an object with
`ASSIGN-POS`/`ASSIGN-KEY`, and otherwise fell out to the generic `Hash`/`Array`
walk — whose root is the object, not a container. The 3+-level walker had no
`Instance` handling at all, so its "autovivify the root itself" arm clobbered
the object.

Nothing was missing on the *production* side. A rw-capable `AT-KEY` body is
already compiled with an rw tail (ADR-0067 slices 1 and 2), so the call already
hands back a `ContainerRef` cell for an existing element, or a `HashEntryRef`
token for one that does not exist yet. That is exactly why the `:=`-bound
spelling of the identical subscript — `my $e := $q<foo>; $e[0] = 99` — has
always produced the right container. The producer existed; it simply was not
consulted.

## What changed

A new `src/vm/vm_lvalue_object_subscript.rs` holds the step:
`object_subscript_accessor` (which accessor an object serves a step with,
extracted from the walker's own primary/secondary probe so both walkers ask the
same question) and `lvalue_object_step_container` (the container a deeper
subscript must walk, given whatever the accessor returned). A location already
holding a container hands that container back — it shares its `Gc` node with the
object's own storage, so the write reaches the object with no write-back — and
an *empty* location autovivifies a container of the kind the **next** step
addresses, which is what makes `$q<new>[0] = 9` grow `{new => [9]}` and
`$p[2][0] = 9` grow the object's array.

Three call sites consult it. The two-level op now calls the accessor exactly
once and keeps both its container and its value, with the `ASSIGN-KEY`/
`ASSIGN-POS` and `Proxy`-element branches unchanged and still taking precedence.
The deep op takes the same step at every intermediate level, keeping each
produced container in a `Vec<Box<Value>>` so its raw-pointer walk has a stable,
kept-alive address to descend into. The generic (stack-computed target) op
gained a `ContainerRef` arm that resolves the cell the way its existing
`HashEntryRef` arm resolves a deferred entry — that is the explicit accessor
spelling `$q.AT-KEY("foo")[0] = 99`, which is not rewritten into a chain-root
temp and so arrived there and was dropped by the catch-all.

The discriminator is the **shape of what the accessor returned**, not a
declaration probe: an accessor that is not rw-capable returns a plain value and
every caller keeps its previous behaviour. The ticket's explicit prohibition —
do not reintroduce an accessor-keyed slow path like the deleted
`__mutsu_index_assign_method_lvalue_nested` — is respected: no new walker was
added, and the chain still goes through the variable-rooted walk.

## A row the design table did not have

`class R { has %.d; method AT-KEY($k) { %!d{$k} } }; $r<foo>[0] = 9` is
`{foo => [9 2]}` in raku even though that accessor is **not** rw: raku mutates
the returned `Array` *object* in place, and a mutsu method return shares its
`Gc` node, so the same holds here. `lvalue_object_step_container` therefore also
accepts a bare `Array`/`Hash` return. Without that row the fix would have read
as "rw accessors only", which is not what raku does.

## Two of slice 5's rows landed here

Measured with `--dump-bytecode` rather than assumed: the method-rooted headline
`$u.query<foo>[0] = 99` compiles to `SetGlobal(__mutsu_lvroot_%query#4)` plus
`IndexAssignExprNested`, so it *is* the variable-rooted walk with the object in
a chain-root temp — and the new branch returns before
`lvalue_root_temp_not_a_container`'s loud refusal can fire. Both it and the
`:=`-bound-alias spelling are green after this slice and are pinned here.

## Pinned

`t/lvalue-subscript-chain-through-object.t`, 16 tests, byte-identical output
under `mutsu` and `raku`: the five acceptance rows, the two `:=`-rooted
spellings, three autovivification shapes, the non-rw accessor, and five
regression rows (the `:=` bind, `push` through an object subscript, an inner
`ASSIGN-KEY` object still winning the outermost write, a plain `Hash` root, and
plain deep autovivification).

## Residuals, measured and left alone

`$q<foo>[0] = 9` where the element holds a bare `Int` dies in raku ("Cannot
modify an immutable Int") and still silently does nothing in mutsu:
`lvalue_object_step_container` declines a location holding a defined
non-container rather than vivifying over real data, so the row is unchanged
rather than newly wrong. And `$a<zz> = 5` on a class supplying `ASSIGN-KEY`
still stores `5` directly instead of dispatching — that is the single-level
named store, a different site from anything this slice touches.
