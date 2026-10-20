# ADR-14144: `does` on a parameter holding a value type writes back through the argument's container

- **Status**: Proposed

## Context

In Rakudo, `$v does R` reblesses the *object* `$v` holds. A `coerce` callback such as `multi sub coerce($v, MyRole) { $v does MyRole }`, handed an array element through a readonly parameter, therefore changes what the caller's element holds. PDF's `t/cos-tie-index.t` relies on this (#14144):

```raku
my role MyRole {};
sub co($v, MyRole) { $v does MyRole }
my @a = 42, 10;
co(@a[1], MyRole);
say @a[1] ~~ MyRole;      # raku: True    mutsu: False
```

mutsu already gets this right for instances. An `Instance` shares its attribute cell, and `does_rebless_instance` retags that cell in place, so every holder sees the role. Since #14163 this includes `Attribute` meta-objects. A value type is different: `Int`, `Str`, `Num`, `Rat` and the rest are immediate `Value`s in mutsu, with no shared node to retag. `$v does R` on one builds a new `Mixin` value and stores it into the parameter only, so the caller's element or variable keeps the plain `10`.

Rakudo's own behaviour here is implementation-shaped rather than specified. It reblesses whichever boxed object the container holds, and that object is sometimes shared further than a user expects:

- `my $b = 5 + 5` after `$a does R`, where `$a = 10`, also does `R`, because both hold the cached small-Int object.
- The literal `10` does not, because it is a different, compile-time object.

Matching that identity graph would mean giving every value-type `Value` heap identity, which mutsu's NaN-boxed immediates were built to avoid (ADR-0001 §7).

## Decision

`does` on a routine's parameter that holds a value type writes the composed value back through the container the argument came from, when there is one.

1. **Record the source only where it can matter.** The compiler marks a routine whose body applies `does` (statement or expression form) to one of its own parameters. For such a routine only, a call keeps each positional argument's source container. The VM already computes these sources for `is rw`/`is raw` binding (`pending_call_arg_sources`). They cover a variable, an array or hash element, or an attribute slot. Every other call pays nothing.
2. **Write back at the `does`.** When `$v does R` composes onto a parameter whose recorded source is a container, the composed value is stored into that container as well as into the parameter. For a value type that value is the `Mixin`. An instance is unaffected: rebless already reaches every holder.
3. **No source, no write-back.** A literal or computed argument has no source container, and there is nothing in the caller to update, as in Rakudo.
4. **Documented divergence.** A *copy* of the value taken before the call (`my $y = $x; co($x)`) keeps the plain value in mutsu, while Rakudo's shared box makes it do the role too. So does the small-Int cache aliasing above. A test pins the divergence, so a later change to it is deliberate.

## Consequences

- PDF's `coerce` callbacks, and any `sub ($v) { $v does R }` helper applied to a variable or element, behave as in Rakudo for the container that was passed.
- Readonly parameters stay readonly. The write-back is the `does` operator's own effect on the object, so `$v = 5` in the body still dies with `X::Assignment::RO`.
- Value types keep their immediate representation. The cost is one compile-time flag per routine, plus the source list for flagged routines only.

## Alternatives considered

- **Give value types heap identity** (box every `Int`/`Str` that might be mixed into). This matches Rakudo's copy case, but every value-type operation would pay for it, and it undoes the NaN-boxing design. Rejected.
- **Bind readonly parameters to a read-only view of the caller's container.** The view would see later writes by the caller during the call, which Rakudo's value binding does not, and every call would pay for it. Rejected.
- **Leave it.** PDF and similar dists stay broken. Rejected.

## Open questions

- Named arguments: a source container is only computed for positionals today. Whether `:$v` needs the same is to be decided when a dist needs it.
- A `does` applied to a parameter inside a nested closure of the routine needs the flag to cover the closure's body too.
