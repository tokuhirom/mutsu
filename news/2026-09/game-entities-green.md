# Game::Entities runs green: custom iterators in `for`, nested destructuring, gather topic leaks

Game::Entities 0.1.6 (an entity-component registry) was drawn from the ecosystem
roulette with both of its baseline test files red. It exposed six unrelated
interpreter gaps, all fixed in one pass:

- **`for` over an object with its own `iterator`.** Rakudo compiles `for EXPR` to
  `EXPR.map(&body, :item(iscont(EXPR)))`, so a bare object that is not in a `$`
  container is iterated through its class's `iterator` override even without
  `does Iterable`. `for $registry.view(Named) { ... }` iterated once over the
  view object instead of over its entries.
- **Nested destructuring.** `-> (:value(($name)), |)`, `-> ($a, ($b, $c))` and
  `-> (:key(($a, $b)), :value(($c, $d)))` dropped the inner pattern in `for`
  signatures; in a `sub` signature a named key handed only its first element to
  the nested pattern, and two nested named patterns were a false
  `X::Redeclaration` on the parser's shared placeholder name.
- **`∈` on Lists read through container cells.** After `@b.sort`, `@b`'s slots
  are aliasing cells, and identity fell through to a structural `eqv`, so an
  equal but distinct List tested as a member.
- **`R.^pun` before `R.new`.** The pun metamethod left the role registered as a
  plain class, so a later `R.new` built an instance without the role markers
  whose `.WHAT` was not `R.^pun`.
- **`gather` body tail value.** The body is compiled as a compilation unit, whose
  tail value went into `$_` — the captured outer one — so every `gather` whose
  body ended in a value overwrote the enclosing topic.
- **Element topics inside `gather`.** `take $_ with %h{$k}` / `given %h<k>`
  looked the container up by name, which missed a `gather` body's captured outer
  lexical and topicalized Nil. The container is now read like any other
  occurrence of the variable.
