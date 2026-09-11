# Test::Async: four interpreter gaps, 8 of 19 modules loading to 14

`Test::Async` 0.1.17 is the ecosystem's canonical `deep_guts` distribution — the
one `docs/ecosystem-guts-dependency-survey.md` names as the reason that class
exists — and its `ecosystem/` record has read `blocked_load` since the corpus
sweep began. Working it as an `ecosystem-dist-fix` run turned up four real,
general interpreter bugs behind that status. None of them is about Test::Async;
each is a construct mutsu got wrong, and all four are pinned by `t/` tests that
rakudo passes identically.

## 1. `class C is export` from a `unit package` that is not the compunit's name

`is export` in rakudo publishes into the **compunit's** `UNIT::EXPORT`, not into
the surrounding package's stash. mutsu instead aliased a module's short type
names into the importer only when the qualified name sat under the *compunit*
name:

```raku
# lib/Foo/Ev.rakumod
unit package Foo;
class Event is export { }
```

The class registers as `Foo::Event`, which is not under `Foo::Ev::`, so the alias
was filtered out and a `use Foo::Ev; Event` answered a bareword `Str` — while the
`sub … is export` beside it imported fine. `run_modules.rs` now reads the
compunit's `is export`-ed type names back out of its parsed AST (the
`__MUTSU_EXPORT_TYPE__` markers the class declarator emits) and admits those
alongside the compunit-prefix rule, restricted to the `unit` package the
declarations actually live in so a transitively loaded dependency's same-named
export cannot claim the alias.

This is the shape of `Test::Async::Event`, and it is why `Test::Async::Aggregator`
and `Test::Async::Reporter` could not see `Event`.

## 2. A role method parameter typed by a class its own body imports

```raku
unit role Foo::Agg;
use Foo::Ev;
multi method event(Event:D $ev) { }
```

mutsu's role-method pre-pass validates parameter type names *before* the role
body's `use` has run, and deferred an unresolvable **qualified** name on that
grounds — but not an unqualified one, which is what an exported type always is.
Result: `Invalid typename 'Event:D' in parameter declaration.` The deferral is now
symmetric, and narrowly so: it fires only when some module the body `use`s is not
yet loaded, so once every such module is in, an unresolvable name is still a typo
and still reports `X::Parameter::InvalidType`.

## 3. `.[0, 1]` — a comma slice on the topic — did not parse

A subscript takes a comma-separated slice list. `$_[0, 1]` and `$x.[0, 1]` both
did; the topic form `.[0, 1]` parsed its index with the single-expression parser,
which stops at the comma, so the `]` check failed and the whole term fell out as a
bare `Confused.` It now uses the same `parse_bracket_indices` the other subscript
forms use. One occurrence of `|.[0, 1]` inside `Test::Async::Base`'s `cmp-deeply`
made that 980-line compunit entirely unparsable.

## 4. A `Metamodel::*HOW` subclass and its `new_type`

Subclassing a builtin metaclass is the documented way to hook type creation, and
`Test::Async::Metamodel::BundleHOW` is textbook:
`unit class … is Metamodel::ParametricRoleHOW;` with a `new_type(|)` override
whose body is `callsame`. Three things were wrong:

- `Metamodel::ParametricRoleHOW` was missing from the list of builtin types a
  user class may name as a parent, so the declaration died with "cannot inherit
  from … because it is unknown" — even though mutsu exposes the type and its own
  `new_type` works. `ClassHOW` and `GrammarHOW` were already there.
- `callsame` from such an override answered `Nil`: the native metamethod was
  offered as the base candidate only while an `EXPORTHOW` DECLARE was in flight,
  so a plain `MyHOW.new_type(:name<X>)` reached the end of the chain with no
  candidate at all. The native `new_type` was factored out of the instance-method
  dispatch into `Interpreter::metamodel_new_type` and is now the base candidate
  in both cases.
- `new_type` on a subclass that does *not* override it died with
  `No such method 'new_type'` — the metamethod was reachable only on the
  `Metamodel::*` type objects themselves.

A type minted through a user HOW now also carries an instance of that HOW as its
`.HOW`, matching rakudo, while the builtin ancestor is still recorded so
behaviour that keys off the metaclass *kind* keeps working.

## Result, and what is left

The load probe over the distribution's 19 provided modules goes from **8 ok / 11
blocked to 14 ok / 5 blocked**. The five that remain are exactly the five declared
with one of Test::Async's own package declarators — `unit test-hub`,
`unit test-bundle`, `unit test-reporter` — plus `Test::Async` itself, which
depends on `Test::Async::Hub`. So the residue is a **single** root cause: a module
defining a *new* package declarator through a slang, which needs
`use NQPHLL:from<NQP>`, `$*LANG.define_slang` with new
`package_declarator:sym<…>` proto-regex candidates, `set_how` to swap the role
metaclass mid-parse, and `QAST`/`$*W` in the actions role. mutsu's ADR-0026 slang
machinery covers *overriding* existing grammar rules (Slang::Tuxic), not adding
declarators. Filed as [#8005](https://github.com/tokuhirom/mutsu/issues/8005)
with the measured repro; the record stays `blocked_load` and the 2026-07-19
scouting note in the survey — which predicted exactly this split — is updated in
place.
