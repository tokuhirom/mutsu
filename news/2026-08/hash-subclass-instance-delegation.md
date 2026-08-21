# `is Hash`/`is Map` subclass instances get a full method-delegation subsystem

`class Bar is Hash {}` used to work only by accident. Investigation started
from a claim that the `is Hash` twin of the `is Array` subclass delegation
subsystem had "the identical gap" as a then-open `ASSIGN-POS` bug — that
turned out to be false: `is Array` subclasses already had a whole delegation
subsystem (the `__mutsu_array_storage` attribute convention,
`Self::is_positional_base`/`positional_base_storage` in
`src/runtime/accessors_state.rs`, and the ~250-line `CallMethodMut`
delegation block in `src/vm/vm_call_method_mut_ops.rs`), and `is Hash`
subclasses had **no equivalent at all**.

Two bugs were bundled together:

1. **The `Bar.new` (zero-arg) construction-time degradation described in the
   original ticket had already been fixed** by unrelated work between the
   ticket being filed and this investigation — `Bar.new` and
   `Bar.new(a => 1, b => 2)` both correctly bless a real `Bar` `Instance` on
   `main` today. What was still broken, and reproduced identically to the
   ticket's severity, was the FIRST subscript assignment on a freshly
   constructed zero-arg instance: `$h1{'a'} = 1` silently replaced the
   variable's value with a bare native `Hash`, discarding the `Bar` class
   identity entirely (`$h1.^name` flipped from `Bar` to `Hash`). Root cause:
   `vm_var_assign_index_named.rs`'s tied-object protection block only
   recognized a user-*declared* `ASSIGN-KEY`/`BIND-KEY` method
   (`has_user_method`), which a plain `is Hash` subclass with no such method
   never has — so the "class declares no subscript-assign method" fallback
   further down unconditionally treated the instance as coercible to a plain
   Hash.
2. **No method delegation existed for any Associative-protocol method** —
   not `AT-KEY`, not `ASSIGN-KEY`, not `keys`/`values`/`kv`/`elems`, not
   `.raku`/`.gist`/`.Str`, not subscript read (`$h2<a>` returned `(Any)`
   instead of the stored value).

## The delegation subsystem

Mirroring the Array subsystem's shape:

- `Self::is_associative_base(name)` / `Self::associative_base_storage(class_key,
  pairs)` in `src/runtime/accessors_state.rs` — the `Hash`/`Map` twin of
  `is_positional_base`/`positional_base_storage`. A subclass of `Map` but
  not also `Hash` (Hash extends Map, so a `Hash` subclass's own MRO always
  contains both) is backed by an immutable Map, matching how the Array twin
  picks a mutable `Array` vs an immutable `List`.
- Construction (`src/runtime/methods_object_dispatch_new.rs`'s default
  constructor and `src/runtime/methods_dispatch_new.rs`'s `dispatch_bless`)
  now populates a `__mutsu_hash_storage` attribute from any constructor
  `Pair` args that don't name a declared attribute, mirroring how the Array
  blocks populate `__mutsu_array_storage` from positional args.
- A new module, `src/vm/vm_hash_subclass_delegate.rs`, houses the
  `CallMethodMut`/`CallMethod`-path delegation (`try_hash_storage_delegate_mut`
  / `try_hash_storage_delegate`), wired into
  `vm_call_method_mut_ops.rs`/`vm_call_method_ops.rs`/
  `vm_call_method_compiled_interpret.rs` right after the existing Array
  blocks. Unlike Array (whose mutators needed hand-written Rust fast paths
  because a plain `real_array` has no rich native coverage of its own), a
  plain `Value::Hash` already has full native method coverage
  (`AT-KEY`/`ASSIGN-KEY`/`BIND-KEY`/`DELETE-KEY`/`EXISTS-KEY`/`keys`/
  `values`/`push`/.../`raku`/`gist`/...) through the same dispatch a named
  `%h` variable uses, so the delegation just re-targets that existing
  dispatch at the storage value through a synthetic env binding — the same
  fallback mechanism the Array block itself already uses for its own
  non-fast-path methods — rather than duplicating native Hash logic.
  `STORE` (bulk-replace, used by the tied-variable declaration/reassignment
  paths) is handled directly since a plain `Value::Hash` has no native
  `STORE` method of its own to delegate to.
- Subscript read (`$h2<a>`) needed no new code at all: the existing generic
  `(Instance, Str)` arm in `vm_var_index_ops.rs` already called
  `try_compiled_method_or_interpret(target, "AT-KEY", ...)` for any
  Instance — it was silently swallowing the "no such method" error because
  no `AT-KEY` delegation existed. Once the delegation landed in
  `vm_call_method_compiled_interpret.rs`, the read path started working for
  free.
- Subscript write (`$h1{'a'} = 1`) needed a new block in
  `vm_var_assign_index_named.rs`, mirroring the existing `__mutsu_array_storage`
  positional-write block, delegating through `ASSIGN-KEY`/`BIND-KEY`.
- `$h<a>++`/`$h<a>--` (`vm_var_assign_post_incdec.rs`), `nextsame`/`nextwith`
  from a user override reaching the native Hash base
  (`native_hash_storage_next_candidate` in `runtime/builtins_dispatch_next.rs`,
  the twin of `native_array_storage_next_candidate`), list-context
  flattening (`runtime/utils/list.rs`'s `value_to_list`), `.gist` rendering
  (`runtime/utils/gist.rs`), the `Associative`/`Map` role check
  (`value/types_isa.rs`), and the Iterable-role dispatch guard
  (`vm_native_dispatch.rs`) all got the same Hash-storage mirror the Array
  case already had.
- The `my %h is Bar = ...` tied-variable declaration gate
  (`vm_var_trait_ops.rs`) and the later-reassignment gate
  (`vm_var_assign_local.rs`'s `instance_is_tied`) only recognized a
  role-composed or user-`STORE`-declaring class; both got a native-MRO
  carve-out (`class_mro(...).any(|n| n == "Hash" || n == "Map")`) mirroring
  the `@`-sigil gate's existing `n == "Array"` check, so `my %h is Bar = ...`
  now blesses a real `Bar` instance instead of falling through to the
  generic `trait_mod:<is>` handler.

## A regression caught and fixed in the same PR

Fixing bug 1 (subscript assignment no longer silently degrading the instance
to a plain Hash) exposed a latent bug in the `:delete` adverb
(`vm_var_delete_ops.rs`): its `DELETE-KEY`/`DELETE-POS` dispatch was ALSO
gated on a user-*declared* method, so `$h<k>:delete` on a native-delegated
`is Hash` instance silently no-op'd once the instance correctly stayed a
real instance instead of degrading. `t/user-class-shadows-immutable-builtin.t`
(`class Map is Hash {}` etc., pinned well before this work) caught it
immediately in the regression sweep. Fixed by adding a `__mutsu_hash_storage`-aware
branch ahead of the `declares`-gated block, delegating to
`try_hash_storage_delegate_mut` directly.

## Verified

Every shape from the original ticket, plus `keys`/`values`/`kv`/`elems`/
`push`/`DELETE-KEY`/`EXISTS-KEY`/`.gist`/`.raku`/`.Str`/string interpolation/
`.list`/`for`-iteration/`~~ Associative`/`~~ Map`/post-inc-dec/`nextsame`,
was diffed byte-for-byte against real `raku` and matched. New regression
coverage: `t/hash-subclass-new-default-ctor.t`,
`t/hash-subclass-mutator-return-self.t`, `t/hash-subclass-vector.t` (mirroring
the existing `t/array-subclass-*.t` files), plus
`t/user-class-shadows-immutable-builtin.t` (pre-existing, now exercises the
`:delete` fix too).

## Known remaining gaps (not regressions, out of scope for this change)

- A declared `has` attribute on an `is Hash` subclass interacts oddly with
  the native `Hash.new`/BUILD flow even in real Rakudo (`class Baz is Hash {
  has $.foo = 42 }; Baz.new(a=>1).foo` is `(Any)` in raku itself, not just
  mutsu) — a pre-existing Rakudo-side subtlety in how `Hash.new` composes
  with subclass attribute defaults, not something this delegation subsystem
  needs to paper over.
- `nextsame`/`nextwith` reaching the native `is Array` base for
  `DELETE-POS`/`ASSIGN-POS` from a user Positional override, and `$a[i]:delete`
  on a plain `is Array` subclass instance with no user `DELETE-POS`, remain
  unfixed on the Array side (pre-existing gaps, confirmed to already exist on
  `main` before this PR, unrelated to the Hash work here).
