# An lvalue subscript chain rooted at a subscriptable object writes through

An lvalue subscript chain that steps through an **object** implementing
`AT-KEY`/`AT-POS` — rather than a plain `Array`/`Hash` — did not reach it. Six
spellings were broken; four of them silently, one by corrupting the object, and
only one by an honest refusal. All of them now match raku.

The design is
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md),
slices 4 (the walkers) and 5 (the method-rooted root). This file was
`todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md`.

## The repro set

All rows use `class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }`
and were re-measured against `raku` v2026.07 before any code was written; every
one still reproduced exactly as the ticket recorded it.

| Spelling | raku | mutsu, before |
|---|---|---|
| `$u.query<foo>[0] = 99` (method-rooted, depth 2 — the headline) | `{foo => [99 2]}` | loud refusal: `Cannot subscript-assign through %!query: it returned Q, ...` |
| `my $q = Q.new(...); $q<foo>[0] = 99` (variable-rooted, depth 2) | `{foo => [99 2]}` | `{foo => [1 2]}` — silent, exit 0 |
| `$u.query<foo> = 99` (method-rooted, depth 1) | `{foo => 99}` | `{foo => [1 2]}` — silent |
| `my $t := $u.query; $t<foo>[0] = 99` | `{foo => [99 2]}` | `{foo => [1 2]}` — silent |
| `$q.AT-KEY("foo")[0] = 99` (explicit spelling) | `{foo => [99 2]}` | `{foo => [1 2]}` — silent |
| `$q<foo><bar>[0] = 99` (depth 3) | `{foo => {bar => [99 2]}}` | `No such method 'd' for invocant of type 'Hash'` — the object was replaced by a Hash |

`$q<foo> = 99`, `my $e := $q<foo>; $e[0] = 99` and `$q<foo>.push(99)` were
already correct and are now regression rows.

## Why it was broken, and why the fix is small

The two-level walker *did* have an `Instance` branch and it *was* entered
(`rust-gdb` breakpoints on the branch, its `Proxy` probe, and the generic walk
below it all fired). It called the accessor as an **rvalue** and discarded its
container on the very next line — `call_method_with_values(...).deref_container()`
— then fell through to a generic `Hash`/`Array` walk whose root is the object,
not a container. The 3+-level walker had no `Instance` handling at all, so its
"autovivify the root itself" arm clobbered the object.

Nothing was missing on the *production* side. A rw-capable `AT-KEY` body is
already compiled with an rw tail (ADR-0067 slices 1 and 2), so the call already
hands back a `ContainerRef` cell for an existing element or a `HashEntryRef`
token for one that does not exist yet — which is precisely why the `:=`-bound
spelling of the identical subscript has always worked. The producer existed and
simply was not consulted.

So the fix consults it, in a shared helper module
(`src/vm/vm_lvalue_object_subscript.rs`) that both walkers, the generic
stack-computed-target store, and the depth-1 method-rooted store now use. No
accessor-keyed slow path was reintroduced — the ticket's explicit prohibition,
because the deleted `__mutsu_index_assign_method_lvalue_nested` and its
copy-on-write rebuild are what dropped the writes this ticket's neighbours were
about.

## What measurement changed about the plan

- **The ADR's slice-5 prescription ("compile the chain root in container mode")
  was unnecessary.** `--dump-bytecode` showed the method-rooted headline already
  compiling to `SetGlobal(__mutsu_lvroot_%query#4)` + `IndexAssignExprNested` —
  the *variable*-rooted walker with the object sitting in a chain-root temp. Two
  of slice 5's three acceptance rows therefore went green with slice 4 alone.
  What slice 5 actually needed was to stop *refusing* an object root
  (`lvalue_root_temp_not_a_container`) and to teach the depth-1 method-rooted
  store — a different function entirely — to call the object's accessor and hand
  the result to ADR-0059's existing consumer.
- **A row the design table did not have.** `method AT-KEY($k) { %!d{$k} }` —
  **not** rw — also writes through in raku, because raku mutates the returned
  `Array` object in place and a mutsu method return shares its `Gc` node.
  Without that row the fix would have read as "rw accessors only", which is not
  what raku does.

## Pinned

`t/lvalue-subscript-chain-through-object.t` (16 tests) and
`t/method-rooted-lvalue-subscript-through-object.t` (12 tests), both
byte-identical under `mutsu` and `raku`, plus the untouched
`t/method-rooted-lvalue-subscript-chain.t`.

## Residuals, measured and deliberately unchanged

Two neighbouring shapes stay exactly as they were rather than becoming newly
wrong: a location holding a defined non-container (`$q<foo>[0] = 9` where
`$q<foo>` is `1`) still silently does nothing where raku dies "Cannot modify an
immutable Int"; and `$w.p<a> = 1` on an object supplying **no** subscript
accessor still reports success where raku dies "does not support associative
indexing" (its depth->=2 twin already refuses loudly).
