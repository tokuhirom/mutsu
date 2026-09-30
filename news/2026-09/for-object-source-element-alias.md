# `for` aliases the elements an object source's iterator yields

A `for` loop over an object used to bind decontainerized copies of its
elements, so a write through the topic or an `is rw` / `<->` parameter was
silently lost. Two shapes now alias like rakudo (#10350):

- an `is Array` subclass (`my @f is Foo = 1, 2; $_ = 5 for @f`) binds the
  slots of its backing storage, the same element containers `@a.values`
  hands out;
- a built-in `Array`'s `.iterator` now yields its element containers, as
  rakudo's `ReifiedArrayIterator` does, so a class whose `iterator` forwards
  to `@!x.iterator` aliases `@!x`, and `my $x := @a.iterator.pull-one; $x = 5`
  writes `@a[0]`. `push-all` / `push-exactly` into an `Array` still copy the
  values.

An element store into an `is Array` subclass (`@f[i] = v`, `@f[i]++`) now
writes through an existing element container instead of replacing it, and
reaches the instance when the variable is captured by a closure — before, the
capture made `@f[0] = 9` truncate `@f` to `[9]` (#10356).

An assignment used as an expression (`if ($v = @a.values[1]) { ... }`, and
the parameter re-bind of `while $it.pull-one -> \r`) now copies an element
container that came off a call, as the statement form already did; it used to
keep the source element's container, so the next assignment wrote into the
source array.
