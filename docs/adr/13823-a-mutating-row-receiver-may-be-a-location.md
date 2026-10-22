# ADR-13823: A mutating row's receiver may be a location

- **Status**: Accepted (user decision 2026-10-22; proposed 2026-10-20, revised 2026-10-21)
- **Deciders**: tokuhirom, Claude
- **Context**: [#13823](https://github.com/tokuhirom/mutsu/issues/13823). Amends the
  `ReceiverPlace` decision of [ADR-11276](11276-built-in-methods-are-handler-rows.md) §8.1
  (`Var` or `Detached`), on top of the slot-addressed binding descriptor of
  [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md), the container producers of
  [ADR-0067](0067-a-routine-hands-back-the-container-it-was-given.md) and the container-mode arguments of
  [ADR-0059](0059-is-rw-routines-return-a-container.md) Slice 3.

## 1. Context

A `Handler::Mut` row (`push`, `ASSIGN-KEY`, `splice`, ...) receives its invocant as a
`ReceiverPlace`. ADR-11276 §8.1 gives that place two shapes:

- `Var`: the call named a binding (`@a.push`, `$x.push`). The row can rebind it, which is how
  `Any`'s autovivifying rows turn `my $x; $x.push(1)` into `$[1]`.
- `Detached`: a container with no binding. The row works on the value and answers it.

A receiver that is neither a binding nor a value has no shape: an element (`@a[1]`, `%h<k>`)
or an `is rw` attribute's container (`$k.attr`). Rakudo autovivifies all of them, because its
`Any:U` candidates assign to whatever container the invocant came in, and it decides that in
the candidate the call dispatches to, not before:

```
$ raku -e 'my @a; @a[1].ASSIGN-KEY("e", 9); say @a.raku;
           my %h; %h<k>.ASSIGN-KEY("e", 1); say %h.raku;
           my @d; @d[0].BIND-KEY("z", 3); say @d.raku;
           class K { has $.attr is rw }; my $k = K.new; $k.attr.push(1); say $k.attr;
           sub f(@x) { @x }; my @c; f(@c)[2].push(5); say @c.raku;
           my @e; my $m = "push"; @e[1]."$m"(4); say @e.raku;
           class P { method push(|) { say "P.push" } }; my @p = P; @p[0].push(1); say @p[0].^name'
[Any, {:e(9)}]
{:k(${:e(1)})}
[{:z(3)},]
[1]
[Any, Any, [5]]
[Any, [4]]
P.push
P
```

mutsu (2026-10-21) answers `X::Method::NotFound` for the three keyed calls, leaves `$k.attr`,
`@c` and `@e` unchanged without an error, and for the last line replaces the `P` element with
an `Array` and never calls `P.push`.

The cause is how an element receiver of a mutating call is compiled today. It is decided by
**method name at compile time**, before dispatch:

- `is_mutating_method_on_index` and `is_nested_mutating_method_on_index`
  (`src/compiler/helpers_ast_utils.rs`) match a hand-written list, `push | pop | shift |
  unshift | append | prepend | splice`, and only when the subscripted target is a named
  variable (`postfix_index_name`).
- For `push`/`append`/`unshift`/`prepend`, `IndexElemAutoviv { autoviv: true }`
  (`src/vm/vm_var_elem_mutate.rs`) stores a fresh `Array` into a missing or type-object
  element **before** the call, then calls the method on it. Any type object that is not one of
  a few listed core types counts as missing, which is why `P.push` is bypassed.
- For `pop`/`shift`/`splice`, the element is stored into a synthetic global
  `__mutsu_tmp_index_method_target_N` and called through `CallMethodMut` with that name
  (`src/compiler/expr_method.rs`).
- Every other method on an element, every unnamed target (`f(@c)[2]`, `(@g)[1]`) and every
  dynamic method name goes through a plain `CallMethod` on the element's value, so a vivifying
  row has nothing to write to.

An `is rw` accessor's result arrives as a value for the same reason, so `$k.attr.push(1)` grows
a fresh array nobody holds.

This is the per-method-name special casing and the synthetic-name binding that §4 rejects, and
it cannot grow to cover the keyed rows: `ASSIGN-KEY` must vivify a `Hash`, not an `Array`, and
must answer the assigned value, so the decision has to be made by the row the call reaches.

## 2. Decision

1. **`ReceiverPlace` gains a third shape, `Cell`**, for a receiver that is a location without a
   binding name:

   ```rust
   pub(crate) enum ReceiverPlace<'a> {
       Var { name: &'a str, value: &'a Value, code: Option<&'a CompiledCode>, arg_sources: .. },
       Cell { cell: &'a Value, value: &'a Value },   // a ContainerRef or a HashEntryRef token
       Detached(&'a mut Value),
   }
   ```

   `cell` is the element's or attribute's container: an existing `ContainerRef`, or the
   deferred `HashEntryRef` token for an element that does not exist yet. `value` is the
   receiver as the call read it, as in `Var`, **including a container default**: with
   `my @b is default(7)`, `@b[2]`'s `value` is `7`, so `@b[2].push(1)` dispatches on `Int:D`
   and fails, as in Rakudo.

2. **A row writes a location and answers a result, separately.** `place.assign(interp, v)`
   stores `v` into the cell through the cell's own store path. That path runs the cell's type
   constraint (`ContainerCell`'s `of`, ADR-0042), realises a deferred token, and takes the
   cell's own synchronisation: a `Cell` is never written through a `&mut` obtained from a
   shared reference (the docs/security.md rule for values another Raku thread can reach). The
   row's return value stays the method's own answer. `Any`'s autovivifying rows use `assign`
   to vivify (`Array` for `push`, `Hash` for `ASSIGN-KEY`) and then run the concrete row on the
   vivified container, exactly as they do for a `Var` today.

3. **The producers are the existing ADR-0067 ones, with a new runtime gate.** Both producers
   are deliberately gated today (an ungated accessor marker measured ~14% on a tight loop), so
   this ADR adds one gate rather than claiming none:
   - **Only an undefined or missing receiver gets a location.** A defined element or attribute
     is passed by value exactly as now; arrays and hashes are already shared nodes (ADR-0013),
     so a row mutating them needs no location. The check is made by the producer at run time
     (one definedness probe of the element or attribute it already reads).
   - **Only a call that could reach a vivifying row.** For a statically named call, the
     producer is enabled when the name is one of the method table's rows that vivify an
     undefined receiver. That set is generated from the rows, never a hand-written list, so a
     new vivifying row is covered when it is added. A dynamic name (`."$m"()`) passes the gate,
     the conservative direction ADR-0067 already takes for `MarkLvalueInvocantRefContext`.
   - The element producer is `IndexInvocantRef`'s `take_subscript_element_cell`. In receiver
     position, under this gate, it hands out the deferred `HashEntryRef` token for a missing
     element instead of declining, as the argument producer does. The token creates the
     element only on the first write, so a call that dispatches elsewhere (`P.push` on a type
     object element) or declines leaves the container unchanged; a plain read never vivifies.
   - The `is rw` accessor producer is the accessor-ref request the `:=` and lvalue-invocant
     paths already emit (`MarkAccessorRefContext`, gated by name like
     `MarkLvalueInvocantRefContext`). A non-`rw` accessor's result stays `Detached`, so
     `$r.ro.push(1)` is `X::Assignment::RO` as in Rakudo.

4. **The name-based element path is retired.** Once the `Cell` producer covers element
   receivers, these go: `IndexElemAutoviv`'s `autoviv` pre-vivification,
   `is_mutating_method_on_index` / `is_nested_mutating_method_on_index` with their name list,
   and the `__mutsu_tmp_index_method_target_N` synthetic global. An element receiver of every
   method then compiles the same way; whether it is written is decided by the row reached.

## 3. Consequences

- Every line of the §1 example matches Rakudo, including the unnamed target, the dynamic name
  and the type-object element whose class has its own `push`. Typed elements and attributes
  (`my Int @a; @a[0].push(1)`) refuse the vivified container with the cell's own
  `X::TypeCheck::Assignment`, the same check a plain `=` performs.
- Every `Mut` row's accessors (`slot`, `assign`, `check_element_types`, `rebound`) gain one
  arm. Rows that do not vivify keep working unchanged, because they read `place.value()` /
  `place.slot()`.
- The ordinary case costs what it costs now: a defined element or attribute is read by value,
  and the gate is one definedness probe plus, for a static name, a compile-time set lookup.
  An undefined receiver pays one token or cell, only when the call can vivify.
- The compiler loses one per-name branch family and one synthetic global, and the `pop`/
  `shift`/`splice` element calls stop going through a named temp.

## 4. Rejected alternatives

- **Per-method writeback branches** (store the receiver back for `ASSIGN-KEY`, the answer for
  `push`, ...). This is the special casing #13823's triage ruled out, and it scales with every
  new mutating row. It is also what §1 shows today's name list already is.
- **Model the element as a `Var` with a synthetic name.** The name-keyed lookups behind `Var`
  (declared type, `is default`, `:=` cells) would then answer for a binding that does not
  exist, which is the identity problem ADR-0097 removes. Today's
  `__mutsu_tmp_index_method_target_N` is this alternative.
- **Always promote the element before the call.** `@a[5].push` on a read-only path would
  vivify, and so would a receiver the row then declines or a class whose own method is called
  (`P.push` above). The deferred token keeps reads side-effect free, as ADR-0059 Slice 3
  requires for arguments.
- **Produce a location for every element and accessor receiver, ungated.** Correct, but each
  `@a[$i].m` and `$o.attr.m` would pay a slot promotion; ADR-0067 measured ~14% for the
  accessor case. The definedness gate removes that cost from every defined receiver.

## 5. Open questions

- Whether `Cell` should carry the descriptor of ADR-0097 once it lands for elements (its
  slice 5, gated behind #8069), so that `is default` and the element type answer through the
  place rather than through the container's metadata.
