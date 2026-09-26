# RepositoryEvent 0.0.3 reaches parity

The ecosystem roulette brought `RepositoryEvent` 0.0.3 from partial to green:
all five test files now pass under mutsu, matching Rakudo's 199 assertions.

The fixes cover two interpreter gaps exercised by the distribution:

- `nqp::create` now gives `Map` subclasses a usable backing store, and
  `nqp::p6bindattrinvres`/`nqp::getattr` can install and expose that store.
- Bare type names in methods resolve through the method's declaration package,
  so a caller's nested type with the same short name cannot capture them.

The latter also preserves lexical scope isolation for classes declared inside
blocks.
