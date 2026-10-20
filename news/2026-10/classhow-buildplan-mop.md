# A class's BUILDPLAN is visible and replaceable through the MOP

Rakudo builds an object by running its class's BUILDPLAN, a list of tasks
derived when the class composes. Modules program against it. AttrX::Mooish
mixes a role into the class's HOW, overrides `create_BUILDPLAN`, reads the plan
through `nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDPLAN')`, and binds a
rewritten one. mutsu constructs natively and had no plan at all (ADR-14162).

mutsu now derives the plan in Rakudo's encoding:

- `0`–`3` set an attribute from a named argument;
- `800` checks `is required`;
- `400`–`403` set defaults;
- `1000` is a no-op;
- a `Code` task stands for `BUILD`/`TWEAK`.

`.^BUILDPLAN`, `.^BUILDALLPLAN` and the nqp attribute ops read it. A HOW
mixin's `create_BUILDPLAN` and `publish_method_cache` overrides run as the
class composes, and `callsame` reaches the native ones. A class whose plan was
bound, or one of whose ancestors had its plan bound, is constructed by running
that plan task by task, its `Code` tasks included. Every other class keeps the
native constructor.

The work also fixed three Proxy cases:

- FETCH now receives the Proxy object itself, so `nqp::decont($proxy)` inside
  it no longer FETCHes again.
- A Proxy subclass whose `new` does `callwith(:$FETCH, :$STORE)` gets an
  instance of itself.
- The result of a `callwith` is no longer FETCHed before the caller sees it.

An `is default(...)` value with no initializer is no longer reported as the
attribute's `.build`.
