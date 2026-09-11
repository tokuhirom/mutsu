# A QuantHash subclass is a QuantHash

`class AccountableBagHash is BagHash { }` produced an ordinary attribute-bag
object. It answered its own name and nothing else: `.total` did not exist,
`.gist` rendered `AccountableBagHash.new`, `%h<a>` read `Nil`, and
`my %abh is AccountableBagHash = a => 42` died with
`Cannot modify an immutable AccountableBagHash`. The whole
[AccountableBagHash](https://360.zef.pm/A/CC/ACCOUNTABLEBAGHASH) distribution —
16 assertions, all of them green under rakudo — could not run a single one.

`is Hash` and `is Array` subclasses have kept their data in a reserved backing
attribute (`__mutsu_hash_storage` / `__mutsu_array_storage`) and delegated the
protocol to it for a long time. The QuantHash family now does the same through
`__baggy_data__`, which already existed but was only ever filled by a
positional `.new` and only ever held an immutable `Bag`:

- `runtime/quanthash_subclass.rs` picks the base out of the MRO and seeds the
  backing store in both constructor paths. The base decides the element
  semantics *and* the mutability, so `class C is BagHash` gets a mutable bag
  and `class C is Bag` keeps the immutable one and still raises on an element
  assignment — the same rule `positional_base_storage` uses to pick `List` over
  `Array`.
- `vm/vm_baggy_subclass_delegate.rs` is the mutating half of the delegation:
  `ASSIGN-KEY`, `DELETE-KEY` and `STORE` write **in place** through the shared
  attribute cell, so every holder of the object — the tied variable, a closure
  that captured it, a `Proxy` over one of its elements — sees the write.
- `.gist` and `.raku` render under the subclass's own name (`MyBagHash(a(2) b)`,
  `("a"=>2).MyBagHash`), and `my %h is <Subclass> = ...` populates the store
  through `STORE(list, :INITIALIZE)` like any other tie.

Four general bugs surfaced on the way, each fixed where it lived rather than
around it:

- **`.STORE` wrote the wrong keys.** The native `BagHash`/`SetHash`/`MixHash`
  `STORE` built its entries from the raw stringification while every reader
  looks them up by the `.WHICH`-derived key, so `$b.STORE(a => 42); $b<a>` read
  `0` on a plain `BagHash`, with no subclass involved. The folding moved to
  `runtime/quanthash_store.rs` and now mints the same keys (and the same
  `original_keys` decoding) as every other write.
- **A single-candidate override had no deferral frame.** A user or role
  override of a container protocol method got no `method_dispatch_stack` frame
  when it was the only candidate, so its `nextsame` answered `Nil` and the
  write vanished. Container subclasses now join the `grammar parse` /
  `BUILDALL` cases that push a frame for their native base candidate, and a
  `multi method` whose candidate list runs out falls back to the same base.
- **A resumed `CATCH` lost its writes at a subscript assignment.** The
  element-assign opcode dispatches `ASSIGN-KEY` without a call opcode, so it
  never drained `pending_rw_writeback_sources` — a handler that ran inline at a
  `.throw` inside the callee and set a flag saw that flag reset on return.
- **`f(...) = v` could not see a `my &f`.** The parser lowers it to
  `__mutsu_assign_named_sub_lvalue("f", [args], value)`, which hides the callee
  behind a string constant: the runtime resolved it against declared routines
  only, and — worse — closure free-var analysis never saw a read of `&f`, so a
  closure that *only* assigned through it did not capture it at all. The
  compiler now retargets the call at the code variable, exactly as it already
  does for a bare `f(...)`.

The last piece is `nextcallee`, which answered `Nil` for every method dispatch.
It now returns the next MRO candidate as a routine, and — when the MRO is
exhausted on a container subclass's `AT-KEY` — the *native* base candidate,
which Rakudo spells as a `Proxy` over the element. mutsu has no routine object
for an opcode, so `runtime/container_element_proxy.rs` supplies one as a small
Raku snippet, the same shape NativeCall's `cglobal` prelude already uses:

```raku
sub ($obj, $key) is rw {
    Proxy.new(
        FETCH => { $obj.__mutsu_container_at_key($key) },
        STORE => -> $, $value { $obj.__mutsu_container_assign_key($key, $value) }
    )
}
```

That makes the documented override idiom work end to end:

```raku
multi method AT-KEY(::?CLASS:D: $key is raw) is raw {
    my &nextone := nextcallee;
    Proxy.new(
      FETCH => { nextone(self,$key) },
      STORE => -> $, Numeric() $value { ... nextone(self,$key) = $value ... }
    )
}
```

`%h<a>++` on such an object reads through the outer Proxy into the inner one
and stores back through both, which also needed value-context reads to fetch a
`Proxy` all the way down rather than one level.

`AccountableBagHash`'s suite is 16/16 now. Pinned by
`t/collections/set-bag-mix/quanthash-subclass-backing.t`,
`t/collections/set-bag-mix/quanthash-store-refill.t`,
`t/collections/subscript/subscript-override-nextcallee.t` and
`t/exceptions/catch-resume-subscript-assign-writeback.t`, all four verified
against rakudo.

One adjacent gap is left open and *not* fixed here: a sigilless lexical
(`\obj`) captured by a `Proxy` STORE closure reads back as its own name. The
prelude above uses `$`-sigiled parameters to step around it.
