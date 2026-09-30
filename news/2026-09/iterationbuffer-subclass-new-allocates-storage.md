# `.new` / `.bless` on an `is IterationBuffer` subclass allocate its element storage

```raku
use nqp;
my class VL is IterationBuffer is repr('VMArray') {
    method STORE(VL:D: \s, :$INITIALIZE) { for s.list { nqp::push(self, $_) }; self }
}
my @a is VL = ^3;
say @a.elems;   # 3   (was: nqp::push: expected a list, IterationBuffer or Buf/Blob, got Any)
```

An `IterationBuffer` keeps its elements in a reserved array attribute
(`__mutsu_iterationbuffer_items`), and nqp code writes into it from the first
statement of a method (`nqp::push(self, ...)`). `Interpreter::create_instance`
seeded that attribute for `nqp::create(VL)`, but `VL.new`, `VL.bless` and the
`my @a is VL` declaration build their instance through the general
constructor paths, which did not. `nqp_backing_array` only vivifies the slot
when the class is exactly `IterationBuffer`, so the first `nqp::push` on a
subclass object found no storage and died.

`seed_native_subclass_payloads` (`runtime/mod.rs`) is the one function both
`Mu.new` and `Mu.bless` call to give a subclass of a native type the reserved
attribute that type needs (`Int`, `Num`, `Str`), and it says so in its doc
comment so the two paths cannot drift. The IterationBuffer slot now lives there
too: any instance whose MRO contains `IterationBuffer` and that has no storage
yet gets an empty real array. Each object gets its own (two `.new` calls do not
share elements), and an object that already carries the slot is left alone.

The rendering of such an object (`say @a` prints `VL.new`, rakudo
`(0, 1, 2).IterationBuffer`) is a separate divergence, filed as #10375.

Pinned by `t/vm/iterationbuffer-subclass-new-storage.t`.
