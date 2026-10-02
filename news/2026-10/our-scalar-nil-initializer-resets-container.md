# `our $x = Nil` resets the container to its default

```raku
our $v = Nil; say $v.raku        # raku: Any   mutsu: Nil
our Int $v = Nil; say $v.raku    # raku: Int   mutsu: Nil
our $v = 9; BEGIN say $v.raku    # raku: Any   mutsu: Nil
```

Assigning `Nil` to a Scalar resets it to its default -- `Any`, or the declared
type's type object -- instead of storing a `Nil` in it. `my $x = Nil` always
behaved that way. `our $x = Nil` did not: a plain `our` scalar with an
initializer is compiled to `OpCode::DeclareOurScalar`, which installs one
shared cell under the lexical slot and every package-qualified name, and its
handler stored the popped initializer verbatim. The cell therefore held a real
`Nil` (`$v === Nil`, `.WHAT` of `Nil`), and for a typed declaration the type
object only showed up because the *read* path converted it.

The handler now turns a `Nil` initializer into the container default on the way
in: the declared type's type object where there is a constraint (`our Int`,
`our Any`, `our Mu` -- the last two used to stay `Nil`), `Any` otherwise. A
definite constraint (`our Int:D $x = Nil`) is left alone: that assignment is a
type error rather than a reset, and this op cannot report one, so its behaviour
is unchanged. `is default(...)` declarations never reach this op.

The BEGIN case needed no separate fix. ADR-0134's prologue gives the static half
of `our $v = 9` a `Nil` initializer while keeping the has-initializer trait, so
it is compiled as `our $v = Nil` and is the same store; a class-body
`our $v = 9` read from a BEGIN (`BEGIN say $A::v.raku`) goes through it too.

Pinned by `t/modules/our-scalar-nil-initializer-resets-container.t` (28 tests,
rakudo's output for each): the untyped and typed scalars, a later `Nil`
assignment, `is default`, bare-block / package / class scopes, and the BEGIN
shapes at file scope.

Found while doing this, not fixed here: with a `BEGIN` anywhere in the file,
the body of a class or module keeps a `Nil` for `my $x = Nil` and `$x = Nil`
(`class A { my $x = Nil; say $x.raku }; BEGIN 1` prints `Nil`; rakudo `Any`).
That is the BEGIN prologue's run-time half of a package body, not the `our`
store, and is filed as
[#10608](https://github.com/tokuhirom/mutsu/issues/10608).
