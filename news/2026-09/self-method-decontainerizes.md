# `.self` decontainerizes, so `$a.self =:= $a` is False again

`.self` looks like it belongs to the raw-invocant method family (`.item`,
`.snitch`) and it does not. In rakudo `.item` hands the invocant's **container**
back — `$a.item =:= $a` is True, and `$a.item = 5` writes `$a` — whereas `.self`
hands back the **value** the container holds. mutsu returned the invocant
unchanged for both, so it answered `True` where raku answers `False`:

```raku
my $a = 42;        say ($a.self =:= $a);   # raku False, mutsu True
class C {}; my $c = C.new;
                   say ($c.self =:= $c);   # raku False, mutsu True
```

The `@`/`%` rows agreed only by accident: those sigils pass the aggregate
itself either way, and the aggregate *is* its own container.

## What `=:=` actually has to decide

mutsu does not reify a Scalar container as a value, so `=:=` is compiled as a
family of specialised opcodes that each recover container identity from what the
operand syntactically *denotes*. `$a.self` denotes no container at all, which
none of the existing spellings could express: the pair fell through to
`ContainerEq(0)` → `values_identical`, and two reads of `42` compare equal.

The new `OpCode::ContainerEqDeconted` carries the *other* operand's variable
name and asks one question at run time: does that name own a Scalar container?
If it does, the answer is False whatever the values are; if it does not, the two
sides are the same thing and the ordinary value compare decides.

Whether a `$` name owns a Scalar is a run-time fact, not a syntactic one, and
the measured table is subtler than "it has a `$`":

| declaration | owns a Scalar? | `.self =:=` |
|---|---|---|
| `my $a = 42` | yes | False |
| `my $i := 42` | no — bound to the literal | True |
| `my $b := @x` | no — aliases the Array | True |
| `my $b := $a` | yes — aliases `$a`'s Scalar | False |
| `my $o := C.new` | no — bound to the object | True |
| non-`is rw` parameter, `for` alias | yes (rakudo reports `Scalar`) | False |

mutsu already recorded two of the three binding spellings — a literal RHS as
`ReadonlyKind::Immutable`, another *variable* in the `__mutsu_sigilless_alias::`
chain, which is also what separates `:= @x` (aliases the aggregate) from `:= $a`
(aliases a Scalar). The third, `my $o := C.new`, was recorded nowhere: mutsu
still lets `$o = 5` through where raku dies, so nothing downstream had needed the
fact. `Interpreter::name_denotes_scalar_container` now consults all three, and
the declaration's store records the missing one as
`__mutsu_scalar_bind_no_container::<name>` — deliberately *not* as a readonly
kind, because it is a container-identity fact and not a writability one, and
widening `:=` writability is a separate divergence.

The `.VAR` handler's own "this name has no container" probe became
`Interpreter::scalar_name_has_no_container`, shared with the new predicate so
the two cannot drift.

## Scope

`.item` is untouched and still hands the container back. `$a.self = 5` already
died before this change. `$o.VAR.^name` still answers `Scalar` where raku
answers `C` — that is the same unrecorded-binding gap seen from the `.VAR` side,
left alone here because flipping it changes a much more widely observed answer.

Pinned by `t/self-method-decontainerizes.t`, whose 22 assertions were each
measured against rakudo 2026.07 and pass identically under both implementations.
