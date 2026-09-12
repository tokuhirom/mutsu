# A plain scalar store decides its flavour once, instead of falling through 2,000 lines

`exec_set_local_op_inner` is the VM's whole-variable store. It is also one long
cascade of store-*flavour* tests — is this a `:=` bind, a rebind, a `constant`, a
declaration, an array share, a `%`/`@` container assignment, an attribute write,
a `Proxy`, a shared cell, a typed lexical, an `is default(...)` variable, an
atomic, a sigilless alias — and `$i = $i + 1` fell through every one of them.
[#8094](https://github.com/tokuhirom/mutsu/issues/8094) measured what that cost:
**746 instructions of self cost per store**, 21.6% of a `while` loop that does
nothing but compare, add and store.

The interesting part of that number is that it is not a hot spot. Nothing in the
cascade is expensive on its own; it is a dozen inert helpers at 20-60
instructions each, and the list reads like a catalogue of questions the store
could have answered without asking:

| per store | what it asked | why the answer was already known |
| --- | --- | --- |
| 58 | `scalar_attr_type_constraint` | the name has no `!`/`.` twigil |
| 36 | `maybe_tied_store_reassign` | the name has no `@`/`%` sigil |
| 27 | `reset_nil_untyped_scalar` | the value is not `Nil` |
| 25 | `update_bound_decont_marker` | no `:=`-bound decont marker exists |
| 20 | `reset_atomic_var_key` | the program has no atomic variable |
| 20 | `mirror_attr_local_to_cell` | the name is not an attribute |
| 20 | `normalize_scalar_assignment_value` | the value is not a 1-element `Seq` |
| 19 | `resolve_pending_alias_binds` | nothing is pending |
| 18 | `term_symbol_from_name`, twice | the name is not `term:<…>` |

Each is individually too small to be worth a local fix, and a local fix would
not help anyway: the cost is the *shape*, not any one line. So the fix is a
single decision taken up front, split in two halves.

The **compile-time half** is a new per-slot bitmap, `CompiledCode::simple_scalar_locals`
— a strict subset of the existing `plain_locals`, adding "not a `term:<…>`
definition" and "not a `__ANON` container slot". A slot in it is an ordinary user
scalar (`my $i` is stored as `"i"`), and its *name* settles every sigil, twigil,
attribute, topic, term and anon branch of the cascade, at the point where the
name is already in hand.

The **runtime half** is `exec_set_local_scalar_fast`, a guard block at the top of
`exec_set_local_op` in which every guard stands in for exactly one branch below
it: the packed mark word is clear (so there is no bind/decl/constant flavour —
and nothing to consume either), none of the monotonic metadata latches is armed
(`bound_array_slice_possible`, `sigilless_readonly_keys_possible`,
`closure_meta_keys_possible`, `atomic_var_seen_anywhere`,
`env_type_constraint_seen`), no collection that would have to be walked is
non-empty (`pending_alias_bind_names`, `local_bind_pairs`, `var_defaults`,
`thread_decl_in_flight`, `our_locals`), and two new pure tag probes say the
incoming value is an ordinary scalar and the slot holds one rather than a cell,
`Proxy` or phantom hash entry. The one env probe that survives is the one the
full path would have run anyway. A guard that fails is never wrong — it just
takes the unchanged slow path, so the cascade remains the single definition of
what a store means.

What the fast path then *does* is three things: itemize the value into its `$`
container (`itemize_scalar_store_value`, minus the name half that the bitmap
settled), write the slot, and take the same `(B)` per-store env-mirror decision
the full path takes.

## Measured

Callgrind, release, `MUTSU_JIT=off MUTSU_GC=off`, the ticket's own control loop
(20,000 iterations of `$i = $i + 1`):

| | before | after |
| --- | --- | --- |
| `SetLocal`, inclusive per store | 1,161 Ir | **312 Ir** (-73%) |
| whole program | 77,867,825 Ir | **60,746,320 Ir** (-22.0%) |

`exec_set_local_op_inner` drops out of the profile entirely for this program —
only the one `my $i = 0;` declaration still reaches it.

## Pin

`t/vm/binding/assign-plain-scalar-store-fast-path.t` drives a plain scalar store
through each branch the guards stand in for — itemization of an Array/Hash/Range/Seq
into a `$`, the `Nil` reset for both an untyped and a typed scalar, a typed
lexical's check, `is default(...)`, a `:=` alias in both directions, a
write-through to an element-bound scalar, a `Proxy`'s `STORE`, an atomic
variable, the topic, an attribute, an `our` mirror, a closure capture, a `state`
accumulator, a `use fatal` `Failure`, and a named sub writing an outer scalar —
and pins that every answer is what it was when every store fell through the whole
cascade. All 28 assertions were verified against the rakudo oracle first.

## Still open

The ticket's second row — a *closure-creation* store at 1,110 instructions — is
narrowed but not closed by this: `my $c = * + 1;` is a declaration, so its
`VARDECL` mark keeps it on the slow path by construction. Hoisting the
declaration flavours the same way is the natural follow-on, and a bigger job,
because a declaration really does have per-slot work to do.
