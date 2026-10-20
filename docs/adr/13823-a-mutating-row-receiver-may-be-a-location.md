# ADR-13823: A mutating row's receiver may be a location

- **Status**: Proposed (2026-10-20)
- **Deciders**: tokuhirom, Claude
- **Context**: [#13823](https://github.com/tokuhirom/mutsu/issues/13823). Amends the
  `ReceiverPlace` decision of [ADR-11276](11276-built-in-methods-are-handler-rows.md) §8.1
  (`Var` or `Detached`), on top of the slot-addressed binding descriptor of
  [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md) and the container-mode
  arguments of [ADR-0059](0059-is-rw-routines-return-a-container.md) Slice 3.

## 1. Context

A `Handler::Mut` row (`push`, `ASSIGN-KEY`, `splice`, ...) receives its invocant as a
`ReceiverPlace`. ADR-11276 §8.1 gives that place two shapes:

- `Var`: the call named a binding (`@a.push`, `$x.push`). The row can rebind it, which is how
  `Any`'s autovivifying rows turn `my $x; $x.push(1)` into `$[1]`.
- `Detached`: a container with no binding. The row works on the value and answers it.

A receiver that is neither a binding nor a value has no shape: an element (`@a[1]`, `%h<k>`)
or an `is rw` attribute's container (`$k.attr`). Rakudo autovivifies all of them, because its
`Any:U` candidates assign to whatever container the invocant came in:

```
$ raku -e 'my @a; @a[1].ASSIGN-KEY("e", 9); say @a.raku;
           class K { has $.attr is rw }; my $k = K.new; $k.attr.push(1); say $k.attr;
           my %h; %h<k>.push(1); say %h.raku'
[Any, {:e(9)}]
[1]
{:k($[1])}
```

mutsu covers part of this with two workarounds:

- An element receiver of `push` is handled by the VM's indexed-method writeback, which stores
  back *what the method answers*. That is right for `push`, whose answer is the grown array.
  It is wrong for `ASSIGN-KEY` and `BIND-KEY`, which answer the assigned value, so
  `@a[1].ASSIGN-KEY("e", 9)` dies with `X::Method::NotFound`.
- An `is rw` accessor's result arrives as a value, so `$k.attr.push(1)` grows a fresh array
  nobody holds, and the attribute stays `Any`.

Neither workaround can grow into the other: the first conflates the method's answer with the
location's new content, and the second has no location at all.

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

   `cell` is the element's or attribute's container (an existing `ContainerRef`, or the
   deferred `HashEntryRef` token for an element that does not exist yet). `value` is the
   receiver as the call read it, as in `Var`.

2. **A row writes a location and answers a result, separately.** `place.assign(interp, v)`
   stores `v` into the cell through the cell's own store path. That path runs the cell's type
   constraint (`ContainerCell`'s `of`, ADR-0042) and realises a deferred token. The row's
   return value stays the method's own answer. `Any`'s autovivifying rows use `assign` to
   vivify (`Array` for `push`, `Hash` for `ASSIGN-KEY`) and then run the concrete row on the
   vivified container, exactly as they do for a `Var` today.

3. **The producers are the existing container-mode ones; no new probe is added.**
   - An element receiver of a mutating call gets its cell from the producer that `@a[0].mut`
     and `IndexArgRef` already share (`take_subscript_element_cell`). In receiver position it
     hands out the deferred token for a missing element only when the row the call reaches is
     a `Mut` row on an undefined receiver. A plain read never vivifies.
   - An `is rw` accessor receiver gets the attribute's cell through the accessor-ref request
     the rw-tail and `:=` paths already emit (`MarkAccessorRefContext`).

4. **The indexed-method writeback for these rows is retired** once the `Cell` producer covers
   them. A `Mut` row never relies on its answer being stored back.

## 3. Consequences

- `@a[1].ASSIGN-KEY("e", 9)`, `%h<k>.push(1)` and `$k.attr.push(1)` match Rakudo. Typed
  elements and attributes (`my Int @a; @a[0].push(1)`) refuse the vivified container with the
  cell's own `X::TypeCheck::Assignment`, the same check a plain `=` performs.
- Every `Mut` row's accessors (`slot`, `assign`, `check_element_types`, `rebound`) gain one
  arm. Rows that do not vivify keep working unchanged, because they read `place.value()` /
  `place.slot()`.
- The answer-versus-location confusion of the writeback is gone for every row, not only for
  the keyed ones.

## 4. Rejected alternatives

- **Per-method writeback branches** (store the receiver back for `ASSIGN-KEY`, the answer for
  `push`, ...). This is the special-casing #13823's triage ruled out, and it scales with every
  new mutating row.
- **Model the element as a `Var` with a synthetic name.** The name-keyed lookups behind `Var`
  (declared type, `is default`, `:=` cells) would then answer for a binding that does not
  exist, which is the identity problem ADR-0097 removes.
- **Always promote the element before the call.** `@a[5].push` on a read-only path would
  vivify, and so would a receiver that the row then declines. The deferred token keeps reads
  side-effect free, as ADR-0059 Slice 3 requires for arguments.

## 5. Open questions

- Whether `Cell` should carry the descriptor of ADR-0097 once it lands for elements, so that
  `is default` on an array (`my @a is default(42)`) answers through the place rather than
  through the container's metadata.
