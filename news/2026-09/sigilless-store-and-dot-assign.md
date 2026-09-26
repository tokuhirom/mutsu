# Sigilless names: assignment reaches a user `STORE`, and `foo.=meth` works as an argument

FixedInt is a class that behaves like a fixed-width integer container: it
declares `method STORE`. Its test file binds one to a sigilless name and then
assigns through it:

```raku
my \foo = FixedInt.new(:8bit);
is(foo -= 1, 255, '8 bit negative ok');
foo += 15;
is(foo.=ror(2), 131, '8 bit rotate right ok');
```

mutsu failed this in two separate ways (#9551, one of the parse gaps split out
of #7988).

**Parse.** Inside parentheses, `foo.=ror(2)` was a syntax error. The postfix
loop sends every bareword `.=` target to the statement-level handler, because
`Foo .= new` on a type object needs that path's read-only check. A sigilless
term is a variable, not a type object, so it now parses like `$x.=meth`.
`wrap_dot_assign` gives it the same bare storage name that `foo += 1` already
writes back to.

**Runtime.** Assigning to a sigilless name bound to a non-container raised
"Cannot modify an immutable value". Rakudo's assignment falls back to calling
`.STORE` on a target that is not a `Scalar`, so an object with a user `STORE`
is its own container. mutsu already routes `%h = ...` / `@a = ...` on a tied
variable (`my %h is Foo`) through `tied_store_dispatch`. That path now also
takes a sigilless name whose bound object has a user `STORE`:

- In statement form, `CheckReadOnly` lets the assignment through and marks the
  name (`pending_sigilless_store`), and the store that follows consumes the
  mark.
- The expression form (`say(foo -= 1)`) emits no `CheckReadOnly`. There, the
  sigilless read-only marker itself identifies the name. Only a sigilless
  binding ever carries that marker, so a `$x` holding the same object is still
  assigned as an ordinary scalar.

A sigilless value without `STORE` (`my \n = 5; n = 6`) still dies with
`X::Assignment::RO`.

One neighbouring gap remains. A sigilless name bound to an *anonymous*
container (`my \foo = my $ = -3`) is still treated as immutable, because the
declaration does not hand the anonymous `Scalar` to the binding. Binding to a
named container (`my \foo = $c`) works.

Pinned by `t/vm/sigilless-store-container-assign.t`.
