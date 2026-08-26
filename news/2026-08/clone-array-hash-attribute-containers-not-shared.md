# `.clone()` keeps sharing an object's Array/Hash attribute containers

Raku's default `Mu.clone` is a *shallow* copy: a `$`-attribute gets its own fresh
container, but an `@`/`%`-attribute keeps pointing at the **same** underlying
`Array`/`Hash` object as the original, so a mutation through either instance's
accessor is visible on both. `Type/Mu.rakudoc` calls this out explicitly ("Hash
and Array attribute modifications in clone appear in original as well").

mutsu's `.clone` was never the bug. Measured on `main`, `$o1.bar =:= $o2.bar` was
already `True` after a clone, and `$o2.bar.push('Z')` already showed up on
`$o1.bar`. What broke the sharing was the very next line of the doc's example:

```raku
$o2.bar = <Z Y>;   # a *whole-container* assignment through the accessor
```

## Root cause

`@a = (…)` in Raku is a list assignment **into** the existing container — it
clears and refills the `Array` the name already holds; it does not rebind the
name to a fresh `Array`. The same is true through a public accessor, because an
`@`/`%`-attribute *is* a container.

mutsu's accessor-assignment paths stored a brand-new container in the attribute
slot instead:

- `assign_method_lvalue_with_values` (`src/runtime/methods_mut_method_lvalue.rs`)
  ended with `updated.insert_through(attr_key, assigned_value)`, and
- the fast accessor arm of `call_method_mut_with_values`
  (`src/runtime/methods_mut_dispatch.rs`) with
  `updated.insert(method, assigned)`.

Replacing the slot silently severed *every* other share of the old container —
most visibly the one `.clone` had just deliberately created, but equally a
closure capture or a second alias of the same attribute.

## Fix

A new value-level primitive, `Value::replace_container_contents`
(`src/value/value_methods_b.rs`), replaces an `Array`/`Hash`'s **contents**
in place through the shared `Gc`, keeping the container's identity and its own
metadata (element/key type constraints, `is default`, declared type). Both
accessor-assignment sites now use it for an `@`/`%`-sigil attribute and return
the *existing* container, falling back to the old slot replacement only when the
two values are not the same container kind.

This is the same "route everything through the one canonical cell" shape the
`our`-variable work established: reads and writes cannot disagree about which
container an attribute names, because there is only ever one.

## The pre-existing leak it exposed, also fixed

Making the accessor assignment write *into* the attribute's container turned an
existing bug into a visible one: `C.new(x => @src)` stored the caller's `@src`
container **as** the attribute, so `$o.x = (9,)` now wrote straight through to
`@src`. That sharing was already wrong on `main` — `$o.x.push(9)` and
`$o.h<z> = 1` leaked into the caller's `@src`/`%hs` too, where raku leaves them
untouched — and this PR fixes the root: an object *owns* its `@`/`%` attribute
containers.

`Value::detached_container_copy` (`src/value/value_methods_b.rs`) makes a fresh
`Gc` copy, and a new `coerce_provided_attr_value_by_sigil`
(`src/runtime/methods_signature.rs`) applies it at the three sites that consume a
**supplied** constructor argument — the native fast path
(`methods_object_default_ctor.rs`), the interpreter's `dispatch_new`
(`methods_object_dispatch_new.rs`), and `bless`'s named-arg mapping
(`methods_dispatch_new.rs`). A *default* expression (`has @.x = 1,2`) is
re-evaluated per instance and already yields the object's own container, so it
keeps the plain `coerce_attr_value_by_sigil` and pays no extra copy.

## Verified

`t/attribute-container-identity.t` pins the whole surface under **both** `raku`
and mutsu: clone shares the `@`/`%` containers (asserted in both `=:=` operand
orders), a push and a hash store through the clone reach the original, an
accessor list-assign *keeps* the identity and is visible on the original, a
`$`-attribute assignment through the clone does **not** reach the original, and
`.clone(:attr(…))` builds a genuinely new container. Two instances of the same
class still never share a container.

## Still divergent (recorded, not fixed)

`$obj.foo` for an `is rw` `$`-attribute returns the attribute's *value* in mutsu,
where Rakudo returns its `Scalar` **container** — so `$o1.foo.VAR.^name` is
`Int` in mutsu and `Scalar` in Rakudo, and `$o1.foo =:= $o2.foo` on two clones is
`True` in mutsu (comparing two identical `Int`s) where Rakudo says `False`
(comparing two distinct `Scalar`s). The value-level behaviour is already correct
either way; making the rw accessor hand back the container is a separate,
larger change, and it is the same root gap behind
`todo/deep/dollar-dot-attr-compound-assign-spurious-ro-error.md`.
