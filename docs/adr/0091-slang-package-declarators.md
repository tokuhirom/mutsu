# ADR-0091: A slang's `package_declarator:sym<...>` candidate is read as a declarator registration, not executed

- Status: Accepted, implemented (2026-09-12)
- Date: 2026-09-12
- Deciders: tokuhirom, Claude
- Extends: [ADR-0026](0026-slang-activation-architecture.md) (slang activation
  — the `$*LANG` handle, the activation sub-interpreter, the recognized-override
  map). ADR-0026 stays in force; this ADR adds a second thing a slang may
  register and keeps §4's refusal to execute Rakudo-internal token bodies.
- Related: [BATTERIES.md](../../BATTERIES.md) §1 (rung 2: grow the interpreter
  until the real module runs verbatim; native provision banned 2026-08-01),
  `news/2026-08/exporthow-declare-mop.md` (the `EXPORTHOW::DECLARE` machinery
  this reuses), [#8005](https://github.com/tokuhirom/mutsu/issues/8005)

## 1. Context

ADR-0026 taught mutsu to run a slang-activating module at parse time and map
the grammar rules its roles **override** onto parser modes. That covers
`Slang::Tuxic`, and hence Text::CSV.

It does not cover a module that **adds** a package declarator. `Test::Async`
does exactly that: `Test::Async::Decl`'s `sub EXPORT` mixes a grammar role
into `$*LANG` that declares three new proto-regex candidates —

```raku
token package_declarator:sym<test-bundle> {
    :my $*OUTERPACKAGE := self.package;
    :my $*PKGDECL := 'role';
    :my $*TEST-BUNDLE-TYPE;
    :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
    :my $*TEST-RESTORE-PACKAGE := True;
    <sym><.kok>
    { $*LANG.set_how('role', Test::Async::Metamodel::BundleHOW); }
    <package_def>
    <.set_braid_from(self)>
}
```

— so that `unit test-bundle Test::Async::Base;` declares a role built with
`BundleHOW`. Five of the distribution's nineteen modules are written with one
of its three declarators (`test-hub`, `test-bundle`, `test-reporter`), and all
five failed to load: mutsu parsed the keyword as a function call. That kept
`Test::Async` at `blocked_load`, and with it the suites of `Async::Workers`,
`Config::BINDish`, `OO::Plugin`, `LibXML::Class` and the `WWW::GCloud*`
distributions, which all test with it.

The issue that filed this ([#8005](https://github.com/tokuhirom/mutsu/issues/8005))
named two routes and asked for a decision:

1. Grow the compiler-guts surface — NQP compunits, a real `$*W` World, a QAST
   IR — until the candidate body can be *executed* the way Rakudo executes it.
2. Recognise `test-bundle` / `test-hub` / `test-reporter` natively as
   role/class declarators, ignoring the NQP `EXPORT` body.

Route 2 is a per-distribution dialect inside mutsu, which BATTERIES.md §1 and
the `ecosystem-dist-fix` skill forbid outright. Route 1 is a different project:
ADR-0026 §4 already rejected executing Rakudo-internal token bodies, for
reasons that have not changed.

## 2. Decision

Take neither. **Read the candidate as a declarator registration.**

Everything in a `package_declarator:sym<...>` candidate that decides what the
new declarator *means* is declarative, and there are exactly three facts:

| fact | where it is written | what it decides |
| --- | --- | --- |
| the keyword | the candidate's `:sym<...>` | the word that introduces the package |
| the package kind | `:my $*PKGDECL := '...'` | `role` builds a role; anything else builds a class |
| the metaclass | `$*LANG.set_how($kind, HOW)` — in the candidate, or in the `EXPORT` body before `define_slang` | the HOW the declaration is built with |

`$*LANG.define_slang` lifts those three out of each candidate its grammar roles
declare and registers a declarator keyword for the rest of the compilation
unit. Everything else in the candidate — `<package_def>`, `<sym><.kok>`,
`<.set_braid_from(self)>`, `HLL::Compiler.lineof`, and the whole QAST-building
actions role beside it — is Rakudo's own compiler surface and is **not** run.
The candidate body is read, never executed; ADR-0026 §4 is preserved verbatim.

The registration then reuses machinery mutsu already has. A registered keyword
parses exactly like `class` (or `role`, per `$*PKGDECL`) and carries a
`__mutsu_declare_how` marker trait naming itself, which is precisely the
representation an `EXPORTHOW::DECLARE` declarator already uses — `monitor`,
from the bundled `OO::Monitors`. The two paths differ only in where the
keyword's HOW is looked up, so a slang declarator gets the metaclass protocol
(`new_type`, `add_method`, deferred `compose`) for free.

### 2.1 Supporting changes

Two things had to change for the registration to be *reachable* at all, and a
third was already in place:

- **`use Foo:from<NQP>;` is a no-op.** It names a compunit in the NQP language;
  mutsu has no NQP compunit repository, since its `nqp::` ops are native.
  Failing the `use` aborted the rest of the `EXPORT` body, which is where the
  entire slang registration lives (`Test::Async::Decl` opens its `EXPORT` with
  `use NQPHLL:from<NQP>;`). Any symbol such a module would have provided stays
  undeclared — the honest outcome, and strictly better than losing every
  declaration that follows.
- **`$*LANG` is bound while a module's `sub EXPORT` runs.** In Rakudo, `EXPORT`
  runs at compile time of the importing unit, where `$*LANG` is live. mutsu
  runs it at module-load time, so nothing bound it and `$*LANG.set_how` died on
  `Nil`. It now gets the same minimal `CompLang` handle the activation
  sub-interpreter uses, so one `EXPORT` body works on both paths.

The third — the ADR-0026 activation gate reaching a module that calls
`$*LANG.define_slang` from its own `EXPORT` rather than through Slangify — was
already needed by the L10N vocabulary work and landed there
(`contains_define_slang` in `parser::stmt::simple::module_exports`, with
Slangify itself excluded because it is the generator, not a slang). This ADR
inherits it: `Test::Async::Decl` reaches the activation path for exactly that
reason.

### 2.2 Where the record lives

An `EXPORTHOW::DECLARE` declarator finds its HOW through an ordinary env
lookup, because the `constant` naming it is declared in the module's mainline
and outlives the load. A slang declarator is registered from `sub EXPORT`,
whose env is restored the moment the call returns — so the record is kept on
the interpreter (`defined_slang_declarators`) and the declaration protocol
falls back to it when the env lookup misses.

## 3. Consequences

- `Test::Async`'s three declarators work, and the two of its five blocked
  modules whose only problem was the declarator (`Test::Async::Hub`,
  `Test::Async::When`) load. The remaining three are blocked by unrelated gaps
  the declarator wall had been hiding, now filed separately:
  [#8023](https://github.com/tokuhirom/mutsu/issues/8023) (a `::`-qualified
  imported type in a role method's parameter) and
  [#8024](https://github.com/tokuhirom/mutsu/issues/8024) (five missing
  `nqp::` ops).
- Any module using the same NQP idiom gets the same support: the mechanism
  keys on what the candidate declares, not on a module or keyword name. That
  is the same "keep the module load-bearing" property ADR-0026 §4 chose over
  name-keyed hardcoding.
- The declarator is unit-scoped, exactly like ADR-0026's rule overrides: an
  `EVAL` string is its own compilation unit and does not see the keyword.
- **What is deliberately not supported**: anything the candidate does beyond
  those three facts. Rakudo's `test-bundle` also runs an `ENTER` phaser built
  in the actions role (`HubHOW.register-bundle`), and restores the swapped HOW
  in a `set_package` override. mutsu runs neither: the HOW swap is per-keyword
  by construction, so it needs no restore, and the phaser belongs to the deep
  MOP surface (`Metamodel::ClassHOW` subclassing with `publish_method_cache`,
  method `.wrap`) that Test::Async needs anyway and mutsu does not yet have.
- **A role-kind declarator's metaclass is recorded but not attached.** In
  Rakudo the swapped HOW is the *individual* role's `ParametricRoleHOW`, while
  `Knob.HOW` is still the group's `ParametricRoleGroupHOW`; mutsu does not
  model that split, so it matches the observable `.HOW` and drops the rest.
  The declaration still carries the `__mutsu_declare_how` marker naming its
  keyword, so the record is there when role-side HOW support arrives. A
  class-kind declarator (`test-hub`) does get its metaclass, which is what
  `Test::Async.HOW.bundles` needs.
- The fixture the behaviour is pinned with (`t/lib/SlangDeclarator.rakumod`,
  `t/modules/slang-package-declarator.t`) is valid Raku and passes 11/11 under
  rakudo as well as mutsu, so the reading above is oracle-checked rather than
  asserted.

## 4. Alternatives considered (rejected)

- **Execute the candidate prologue** (parse the `:my` declarations and `{...}`
  blocks out of the candidate and *run* them, instead of reading them):
  marginally more general — a computed `set_how` argument would work — but it
  requires per-statement error tolerance, because the same prologue also holds
  `:my $*OUTERPACKAGE := self.package;` and `HLL::Compiler.lineof(...)`, which
  cannot run outside a real parse. Swallowing errors in a registration path
  turns a broken slang into a silently missing keyword. The declarative read is
  deterministic and fails visibly.
- **Route 1 from the issue — NQP compunits, `$*W`, QAST**: rejected as ADR-0026
  §4 rejected it, and for the same reason. It is a different project, and
  nothing in the corpus needs the *behaviour* of a candidate body, only its
  registration.
- **Route 2 from the issue — recognise Test::Async's three keywords natively**:
  banned by BATTERIES.md §1 (2026-08-01 user decision). It would also be dead
  weight the moment a second distribution added a declarator.
- **Keying on the module name** (`Test::Async::Decl` ⇒ install three
  declarators): name-keyed native provision in disguise, rejected by ADR-0026
  §4 for `Slang::Tuxic` on the same grounds.

## 5. Open questions

- The declarator is registered for the importing unit only. Rakudo scopes it
  lexically (a `use` inside a block ends with the block); mutsu's declarator
  table is unit-scoped, the same approximation the `EXPORTHOW::DECLARE` table
  already makes.
- A candidate that declares a package kind mutsu has no declarator for
  (neither `class`-like nor `role`) is built as a class. No corpus module does
  this today.
