# An `is Array` subclass now supports the `is` trait, `nextwith`, and fractional subscripts

Investigating `Array::Rounded` (`todo/tickets/dist-test-suite-failures-batch.md`) surfaced four
separate, general bugs in how a user class that `is Array` interacts with the rest of the
interpreter — all confirmed against real `raku` and fixed:

1. **`Foo.new(1,2,3)` with no user-defined `new`** went through the generic default-constructor
   path (`dispatch_new`'s positional-arg handling in `methods_object_dispatch_new.rs`), which
   stashed the elements under a stray `__array_items` attribute key that none of the Array
   delegation methods (`elems`, `AT-POS`, `push`, list-context flattening, ...) actually read —
   they all read `__mutsu_array_storage`, the same key the `nextwith(|@values)`-from-`new` path
   already used. `Foo.new(1,2,3).elems` silently came back `0`. Fixed by using the same key and the
   same `Value::real_array` constructor; the now-dead `__array_items` read in
   `runtime/utils/list.rs` was removed.
2. **`my @a is Foo = ...`** (the `is` trait on an `@`-sigil variable) had no implementation at all
   for a user class target — `exec_apply_var_trait_op` (`vm_var_trait_ops.rs`) only handled the
   `%`-sigil "tied hash" case. Added a mirroring `@`-sigil branch: when the trait names a registered
   class/role whose MRO includes `Array`, composes `Positional`, or defines `AT-POS`, construct an
   instance of it and gather any initializer already assigned to the variable as positional args.
3. **`nextwith`/`nextsame` from inside a single (non-multi, non-wrapped) compiled method** — which
   pushes no `method_dispatch_stack` frame — fell straight through to the "MRO exhausted, return
   Nil" fallback in `dispatch_next_candidate` (`builtins_dispatch_next.rs`) instead of reaching the
   native `Array` base behavior. `Array::Rounded`'s `method AT-POS($i) { nextwith $i.round }` (called
   directly, e.g. `$obj.AT-POS(1.5)`) silently returned `Nil`. Added
   `native_array_storage_next_candidate`, mirroring the existing `native_mu_base_next_candidate`
   fallback, which resolves the invocant from the samewith context / `self` env binding (there is no
   dispatch frame to read it from) and delegates to the native method on the backing storage.
4. **A fractional/`Rat` subscript (`@obj[1.5]`) on an `Instance`** fell through every match arm in
   `vm_var_index_ops.rs` straight to `Nil` — only an `Int` index dispatched to `AT-POS`/`AT-KEY`.
   Confirmed against raku that subscript syntax always truncates a fractional index to `Int` before
   `AT-POS` ever sees it (`$obj[1.5]` calls `AT-POS(1)`, not `AT-POS(1.5)` — only a *direct*
   `.AT-POS(1.5)` call sees the fractional value). Added a truncating arm mirroring the plain-`Array`
   truncation behavior, instead of forwarding the untruncated value (which would have diverged from
   raku semantics for the general case).

Regression-pinned in `t/array-subclass-new-default-ctor.t`, verified line-for-line against `raku`.

**Not fixed, and still blocking `Array::Rounded`'s own test suite** (16/35 still fail): the dist's
actual rounding mechanism is a set of exported `multi sub postcircumfix:<[ ]>` candidates (an
operator overload, not the `AT-POS` method), which mutsu never dispatches for `@obj[...]` syntax —
see `todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`. A separate gap (`my @a is
Rounded = ...` where `Rounded` is a constant imported from another module) is also still open,
documented in the same ticket.
