# An `IterationBuffer` renders as its elements, not as `IterationBuffer.new`

```raku
use nqp;
my $b := nqp::create(IterationBuffer);
nqp::push($b, 1);
nqp::push($b, "two");
say $b.raku;   # (1, "two").IterationBuffer   (was IterationBuffer.new)
say $b;        # (1, "two").IterationBuffer
```

An `IterationBuffer` keeps its elements in a reserved array attribute, and the
generic instance renderer that answers `.raku` and `.gist` for a class without
a `raku`/`gist` of its own printed `Class.new` from the declared attributes. It
never looked at that slot, so a buffer with contents rendered as the empty
constructor (#10375, found while fixing #10351, which made `is IterationBuffer`
subclasses usable in the first place).

Rakudo's form is the elements as a **List** `.raku`, then `.IterationBuffer`:
`().IterationBuffer` when empty, `(5,).IterationBuffer` for one element (the
List trailing comma), `(1, "two", 3.5, (4, 5), Any).IterationBuffer` in general.
`.gist` is the same text, because rakudo renders the elements with `.raku` in
both, so a string element is quoted in `.gist` as well. The renderer
(`runtime/methods_instance_ops.rs`, next to the `is Array` / `is Hash` storage
delegation it resembles) builds a List from the slot and asks it for its `.raku`
through normal dispatch, so a nested buffer, or an element whose class has its
own `raku`, renders correctly. An `is IterationBuffer` subclass renders through
the same method, and so under the base type's name, exactly as rakudo does
(`VL.new` with one push is `(5,).IterationBuffer`, while `.WHAT` is still `VL`).
A class that declares its own `raku` keeps it, and `.Str` (`IterationBuffer<addr>`)
is unchanged.

The issue text gave the single-element form as `(5).IterationBuffer`; measured
against rakudo it is `(5,).IterationBuffer`, which is what the test pins.

Pinned by `t/collections/lazy-seq/iterationbuffer-raku-gist.t`, whose
expectations were taken from rakudo.
