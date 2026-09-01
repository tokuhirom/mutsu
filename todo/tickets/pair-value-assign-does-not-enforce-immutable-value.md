# `Pair.value = X` succeeds where raku throws "Cannot modify an immutable Str"

## Symptom

Raku's `Pair.value` is an `rw` accessor onto whatever the Pair holds, so
assigning through it is only legal when the Pair's value is itself a container.
A Pair built from a literal holds the bare value, and the assignment throws:

```
$ raku -e 'my $p = (1 => "a"); $p.value = "z"; say $p'
Cannot modify an immutable Str (a)
```

mutsu accepts the assignment and mutates the Pair in place:

```
$ mutsu -e 'my $p = (1 => "a"); $p.value = "z"; say $p'
1 => z
```

It is not specific to a bare scalar. The same gap shows through an array
element and through a `for` loop's topic:

```
$ raku  -e 'my @t = (1 => "a"); @t[0].value = "z"; say @t'   # Cannot modify an immutable Str (a)
$ mutsu -e 'my @t = (1 => "a"); @t[0].value = "z"; say @t'   # [1 => z]

$ raku  -e 'my @t = (1 => "a"), (2 => "b"); .value = "z" for @t; say @t'
Cannot modify an immutable Str (a)
$ mutsu -e 'my @t = (1 => "a"), (2 => "b"); .value = "z" for @t; say @t'
[1 => z 2 => z]
```

The mirror case is right, which is what makes this a missing *check* rather
than a missing feature: `my $v = 1; my $p = (1 => $v); $p.value = 2` must
succeed and write through to `$v`, and a Pair whose value came from a container
already behaves that way in mutsu.

## Why this is not ADR-0045's

Found during ADR-0045 slice 6's sweep, while checking that the new `Pair` arm
of `loop_var_unchanged` had not changed Pair semantics. It had not: the
divergence reproduces with no loop at all (the first snippet above), so it is a
`Pair.value` lvalue gap, not a for-loop one. It is recorded here rather than in
the ADR so the ADR's closeout is not held up by it.

## Where to look

The `.value` lvalue path — `runtime/methods_mut_method_lvalue.rs` and the
`Pair` arm of the method-lvalue dispatch. What is missing is the same
"is the target a container?" gate that a plain `1 = 2` assignment already
applies: a Pair holding a bare `Str`/`Int` has no container to assign into, so
the store must raise `X::Assignment::RO` ("Cannot modify an immutable Str (a)")
instead of replacing the Pair's value slot.

[ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
is the neighbouring decision: its slice 3 deferred `.pairs` precisely because a
`Pair` wrapping an element leaks its container through `Pair.value` consumers,
and `todo/deep/pairs-element-containers-leak-through-pair-value-consumers.md`
carries that thread. This ticket is the *other* direction — a Pair with **no**
container behind its value — so the two should be checked together but are not
the same bug.

## Repro to pin when fixed

```raku
dies-ok { my $p = (1 => "a"); $p.value = "z" }, 'a bare Pair value is immutable';
lives-ok { my $v = 1; my $p = (1 => $v); $p.value = 2 }, 'a container Pair value is assignable';
{
    my $v = 1;
    my $p = (1 => $v);
    $p.value = 2;
    is $v, 2, 'the assignment writes through to the container';
}
```
