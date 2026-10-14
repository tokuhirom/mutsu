# Iterable instances in set coercions; `nqp::p6bindattrinvres` builds a Capture

ValueList's `t/01-basic.rakutest` now produces the same TAP as under rakudo.
Two general gaps were in the way:

- A user class that composes `Iterable` was a single opaque element to
  `.Set`/`.Bag`/`.Mix` (and their `Hash` forms) and to every set operator, so
  `@a.roll (elem) @a` on a `my @a is ValueList` was `False`. Rakudo's `Any.Set`
  and the set operators read the object's `iterator`; mutsu now expands such an
  instance through the same element source `for` and list assignment use
  (`try_iterable_instance_items`). A class with only an `iterator` method and
  no `Iterable` still counts as one element, as in rakudo.
- `nqp::create(Capture)` handed back an attribute bag rather than a Capture,
  and `nqp::p6bindattrinvres($capture, Capture, '@!list', $list)` never reached
  the Capture's storage, so ValueList's `.Capture` was a `Capture.new` holding
  nothing. `nqp::create(Capture)` is now an empty Capture, and binding `@!list`
  / `%!hash` answers the Capture with those positionals / nameds -- the write
  side of the Capture arm `nqp::getattr` already had.
