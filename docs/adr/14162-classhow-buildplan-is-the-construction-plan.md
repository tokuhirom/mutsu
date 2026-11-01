# ADR-14162: A class's BUILDPLAN is its construction plan, visible and replaceable through the MOP

- **Status**: Accepted

## Context

In Rakudo, an object is initialized by the class's *BUILDPLAN*. `Metamodel::BUILDPLAN.create_BUILDPLAN` builds it once, when the class composes. It is a list of tasks stored in the `ClassHOW`'s private attributes `@!BUILDPLAN` (this class) and `@!BUILDALLPLAN` (the whole MRO). `BUILDALL` runs the tasks in order. A task is one of two things:

- an nqp list `[code, type, '$!name', ...]`. The task code says what to do with one attribute. Measured on Rakudo v2026.09: `0` sets it from the matching named argument, `400` evaluates its default, `800` checks `is required`, `1000` leaves it untouched. Older Rakudos used `4`/`8`/`10`, and AttrX::Mooish picks the encoding from `$*RAKU.compiler.version`.
- a `Code` object, such as a `BUILD` or `TWEAK` submethod or any block, called with the instance and the named arguments.

For example, `class A { has $.a = 1; has $.b is required; has $!c; has Int $.d; submethod TWEAK {} }` has this plan:

```
[0, A, '$!a', 'a']   [0, A, '$!b', 'b']   [0, A, '$!d', 'd']
[800, A, '$!b', …]   [400, A, '$!a', …]   [1000, A, '$!c']   [1000, A, '$!d']
TWEAK
```

The plan is part of the meta-object protocol, and ecosystem modules program against it. AttrX::Mooish (#14162) is the clearest case:

1. Its `trait_mod:<is>(Attribute, :mooish)` mixes a role into the class's HOW (`$*PACKAGE.HOW does AttrX::Mooish::ClassHOW`).
2. That role overrides `create_BUILDPLAN(Mu \type)`. It `callsame`s, then reads the plan with `nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN')`, and puts a `Code` task where each mooified attribute's init task was (`.make-mooish($instance, type, %attrinit)`, which installs a lazy `Proxy`). It also rebuilds `@!BUILDALLPLAN`, and binds both back with `nqp::bindattr`.
3. It overrides `publish_method_cache` to add a `clone` method before the class's method cache is published.

mutsu has no BUILDPLAN. Construction is native Rust: `NativeCtorPlan`, the per-class cache that `try_native_default_construct` reads. It covers default construction completely, but no Raku code can see it, override it or run through it. So none of AttrX::Mooish's hooks ever fire, and even `has $.bar is mooish(:lazy)` reads `(Any)` (#14162). Its suite passes 0-18 assertions per file where Rakudo passes all of them. Any distribution that customizes construction through the HOW is in the same position.

`AGENTS.md` bans providing such a module natively (ADR-0096 rung 3). mutsu has to grow the protocol until the vendored AttrX::Mooish runs unchanged.

## Decision

Make the BUILDPLAN mutsu's construction plan in the same encoding Rakudo uses, and let the MOP read and replace it. Construction keeps its native speed while nothing has replaced the plan.

1. **One plan builder.** One function, `build_plan_tasks(class)`, mirrors `Metamodel::BUILDPLAN.create_BUILDPLAN`. It derives the task list from the class shape that `NativeCtorPlan` already collects: attribute init from named arguments, defaults, `is required`, `is built`, `BUILD`/`TWEAK`, and the `1000` "leave alone" entries. It uses the current Rakudo's task codes (`0`/`400`/`800`/`1000`, …), and `$*RAKU.compiler.version` reports a Rakudo version that selects that encoding. `NativeCtorPlan` keeps its role as the derived fast-path cache. The task list is a second view of the same facts, never a parallel model.

2. **The plan lives on the class's HOW.** `nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDPLAN')` and `'@!BUILDALLPLAN'` return the plan as an nqp list of tasks. `nqp::bindattr` with those names stores a *replacement* plan on the class. These two attribute names are served by the same `nqp::getattr` arm that already models other `Metamodel::*` private attributes.

3. **`create_BUILDPLAN` and `publish_method_cache` are real methods of `Metamodel::ClassHOW`** (method-table rows, ADR-11276), so a HOW mixin's override can `callsame` into them. Composition calls them through ordinary method dispatch on the class's HOW only when the HOW is a mixin type (`$how does R`). For every other class, composition keeps the native path with no dispatch.

4. **Construction branches once on "plan replaced".** A class with no bound replacement constructs exactly as today, natively. A class whose plan was replaced runs a small plan interpreter. It executes each task by its code, the same way `BUILDALL` does, and calls each `Code` task with `(instance, |%attrinit)`. The interpreter runs bytecode-compiled code throughout (no tree-walk fallback): the task semantics are the native constructor's own helpers, called per task.

5. **Rakudo's encoding is the contract.** Task codes, the order of entries, and the `[code, type, name, ...]` layout follow Rakudo's `BUILDPLAN.nqp`. A module that inspects or rewrites the plan sees what it would see on Rakudo. A divergence is a bug against this ADR, pinned by a test that compares with `raku`.

## Consequences

- AttrX::Mooish, and other dists that customize construction through the HOW (lazy attributes, attribute aliases, triggers), can run unchanged, as ADR-0096 requires.
- Classes that never replace their plan pay nothing: same native path, one extra flag check per construction.
- The plan interpreter is a second construction engine. It must reuse the native constructor's per-attribute helpers (type checks, container creation, `is default`, smileys) so the two engines cannot drift. This is the main maintenance cost.
- `$*RAKU.compiler.version` becomes load-bearing for task encoding. Changing it needs a check that the encoding still matches.

## Alternatives considered

- **Native AttrX::Mooish.** Banned by ADR-0096 (rung 3).
- **Always construct through the plan interpreter.** One engine, but every construction would pay task dispatch. The native path is the hot path that `NativeCtorPlan` was introduced to make fast (#12917). Rejected unless measurements show the interpreter is as fast.
- **Expose a synthetic plan read-only, ignore `bindattr`.** Lets a module read the plan but drops its changes, which is exactly what AttrX::Mooish needs to make. Rejected.

## Open questions

- The full list of current task codes and their payloads (`400` with its default thunk, `800` with its exception data, the `1100`+ codes for `is built`, native attributes and default values of different kinds). It has to be taken from Rakudo's `BUILDPLAN.nqp` before implementation.
- Whether `@!BUILDALLPLAN` is needed as separate state or can be derived from the MRO's `@!BUILDPLAN`s on demand. AttrX::Mooish writes both.
- Which other `Metamodel::ClassHOW` compose-time hooks dists override (`compose_attributes`, `incorporate_multi_candidates`) and whether they belong to the same slice.

## Constructor binding follow-up (#14559)

The 1300/1400 task codes and `BuiltTrait::Bind` are recorded, but construction
still treats them as assignment. On 2026-11-01, a focused oracle comparison
confirms that Rakudo binds both a provided scalar and a default expression's
scalar container; a literal instead becomes immutable. Array/hash element
arguments, `bless` forwarding, cached constructor calls and cells escaping their
declaring frame obey the same rule. Ordinary attributes continue to assign into
their own storage. The regression is
`t/oo/construct/buildplan-built-bind-containers.t` (Rakudo passes all 15 cases;
mutsu initially fails 12).

Implementation follows the existing container transport contract (ADR-0067):
retain the named argument's container at its producer, then let the attribute's
construction mode choose binding or value assignment. Reuse the attribute
`ContainerRef` storage and bound-value marker used by explicit `$!attr := ...`.
The native constructor, `bless` and replacement-plan task runner must share
this operation. A bound default is compiled in container-returning mode instead
of recovering a location from its value after evaluation.

The implementation uses an MRO-resolved `attr_binds` fact in `NativeCtorPlan`;
the attribute's construction mode is classified once per class shape. Bare
bound values sit in readonly binding cells, reusing the normal store refusal,
while supplied cells and Proxies remain the actual storage location. The
compiler records a named readonly parameter's alias kind on `MakeNamedArg` so
light routine calls need no new name-keyed readonly bookkeeping.

Ordinary native attribute initialization is moved into
`constructor_named_attrs.rs`, the generic assignment and `bless` seed pass into
`constructor_assignment.rs`, and common binding into `constructor_binding.rs`.
Pair bytecode handlers live in `vm_pair_ops.rs`. The expanded regression covers
35 passing oracle cases, including all construction engines, aggregate-holding
Scalars, Proxy STORE, inherited modes, source container type constraints,
readonly parameters and type-object refusal. Checked writes through cells use
the shared readonly-kind error constructor instead of flattening every refusal
to the immutable-value message.
