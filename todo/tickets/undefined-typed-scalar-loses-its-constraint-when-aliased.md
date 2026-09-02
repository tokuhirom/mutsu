# An UNDEFINED typed scalar loses its constraint when it is aliased

A `:=` alias of a typed scalar carries the source's `of`-type, so a write
through the alias is type-checked exactly like a direct store — but only when
the source container already holds a value:

```
# defined source: both agree
raku  -e 'subset S of Int where * < 128; my S $c = 5; my \y := $c; y = 1000'
mutsu -e 'subset S of Int where * < 128; my S $c = 5; my \y := $c; y = 1000'
# both: Type check failed in assignment to $c; expected S but got Int (1000)

# undefined source: mutsu loses the constraint AND the container
raku  -e 'subset S of Int where * < 128; my S $c; my \y := $c; y = 1000'
# X::TypeCheck::Assignment
mutsu -e 'subset S of Int where * < 128; my S $c; my \y := $c; y = 1000'
# X::Assignment::RO, then "Cannot modify an immutable Package ((S))"
```

An uninitialized typed scalar holds its type OBJECT (`S`), and the alias binds
that value rather than the variable's container, so the write is refused as a
store into an immutable package instead of being type-checked against `S`.

The same gap shows up through a `for` loop parameter, which is how it was
found:

```
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a;
for $a -> \x { x = 1000 }      # raku: X::TypeCheck::Assignment; mutsu: silently sets $a to 1000
```

(The loop's *write-through* itself is correct now — see
`news/2026-09/for-list-multi-param-source-writeback.md`. Only the check is
missing, and the single-variable declaration form above shows the loop is not
the cause.)

## Where to look

`OpCode::MarkSigillessBind` (`src/vm/vm_exec_dispatch.rs`) decides a sigilless
term's writability from what it is bound to, and a type object is (correctly)
not writable. The question is one level earlier: an undefined `my S $c` should
still present a CONTAINER to the bind, tagged with its `of`-type the way
`register_container_constraint_named` tags a promoted cell, instead of handing
over the bare type object. Compare the defined case, which does exactly that.

The loop-parameter spelling additionally goes through the source-variable
writeback (`vm_loop_writeback_quant.rs`) rather than an alias, so it bypasses
any container check; making the loop parameter a real alias of the source
variable's container would fix the check there for free, and is the shape the
retired ticket
(`news/2026-09/for-list-multi-param-source-writeback.md`) points at.

## Provenance

Split out of `todo/deep/for-loop-pointy-sigilless-param-write-through-missing.md`
on 2026-09-02 when its write-through half was fixed. That file named
`Native::Overflow`'s `t/01-basic.rakutest` as the consumer that needs the
type-check half.
