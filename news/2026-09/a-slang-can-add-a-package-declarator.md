# A slang can add a package declarator

ADR-0026 taught mutsu to run a slang-activating module at parse time and map the
grammar rules its roles **override** onto parser modes. A module that **adds** a
package declarator — a new keyword that introduces a package the way `class` and
`role` do — was still out of reach, and that was the single wall keeping
`Test::Async` at `blocked_load`.

`Test::Async::Decl`'s `sub EXPORT` mixes a grammar role into `$*LANG` that
declares three proto-regex candidates:

```raku
token package_declarator:sym<test-bundle> {
    :my $*OUTERPACKAGE := self.package;
    :my $*PKGDECL := 'role';
    :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
    <sym><.kok>
    { $*LANG.set_how('role', Test::Async::Metamodel::BundleHOW); }
    <package_def>
    <.set_braid_from(self)>
}
```

so that `unit test-bundle Test::Async::Base;` declares a role built with
`BundleHOW`. Five of the distribution's nineteen modules are written with one of
its declarators, and mutsu parsed the keyword as a function call:

```
$ ./target/debug/mutsu -I "$DIST/lib" tmp/tb.raku
WARNING: could not find module Test::Async::Decl to use, ignoring
  in block <unit> at .../lib/Test/Async/Decl.rakumod line 34
Unknown function: test-bundle
```

## What changed

[ADR-0091](../../docs/adr/0091-slang-package-declarators.md) takes neither of
the two routes the issue named — not a compiler-guts campaign (NQP compunits, a
real `$*W` World, a QAST IR), and not the per-distribution dialect BATTERIES.md
§1 forbids. It **reads** the candidate.

Everything in such a candidate that decides what the declarator *means* is
declarative, and there are exactly three facts: the keyword is the `:sym<...>`,
the package kind is `:my $*PKGDECL := '...'`, and the metaclass is whatever
`$*LANG.set_how` names — either inside the candidate or in the `EXPORT` body
before `define_slang`. `define_slang` lifts those three out and registers a
declarator keyword for the rest of the compilation unit. Everything else —
`<package_def>`, `<.set_braid_from(self)>`, `HLL::Compiler.lineof`, and the
QAST-building actions role beside it — is Rakudo's own compiler surface, and is
read, never run. ADR-0026 §4's refusal to execute those bodies is preserved
exactly.

A registered keyword then parses like `class` (or `role`, per `$*PKGDECL`) and
carries the same `__mutsu_declare_how` marker trait an `EXPORTHOW::DECLARE`
declarator already uses — `monitor`, from the bundled `OO::Monitors` — so the
metaclass protocol (`new_type`, `add_method`, deferred `compose`) comes for
free.

Two supporting gaps had to close first, each general in its own right:

- **`use Foo:from<NQP>;` is a no-op** instead of a hard "Could not find"
  failure. It names a compunit in the NQP language, and mutsu has no NQP
  compunit repository because its `nqp::` ops are native. This mattered because
  of *where* the idiom sits: `Test::Async::Decl` opens its `sub EXPORT` with
  `use NQPHLL:from<NQP>;`, so failing there silently discarded the whole slang
  registration below it.
- **`$*LANG` is bound while a module's `sub EXPORT` runs.** In Rakudo, `EXPORT`
  runs at compile time of the importing unit, where `$*LANG` is live; mutsu runs
  it at module-load time, so nothing bound it and `$*LANG.set_how` died on
  `Nil`. It now gets the same minimal `CompLang` handle the parse-time
  activation sub-interpreter uses, so one `EXPORT` body works on both paths.
A third — the activation gate reaching a module that calls
`$*LANG.define_slang` from its own `EXPORT` rather than through Slangify —
was already in place from the L10N vocabulary work, and is what gets
`Test::Async::Decl` onto the activation path at all.

## Result

The repro from the issue is silent, matching rakudo:

```
$ ./target/debug/mutsu -I "$DIST/lib" tmp/tb.raku ; echo "exit=$?"
exit=0
```

Two of `Test::Async`'s five blocked modules now load — `Test::Async::Hub` (its
`unit test-hub` declaration and the `HubHOW` metaclass reached through `.HOW`)
and `Test::Async::When`. The other three are blocked by unrelated gaps that were
hidden behind the declarator wall and are now separately visible: two need a
`::`-qualified imported type to be accepted in a *role* method's parameter
([#8023](https://github.com/tokuhirom/mutsu/issues/8023) — the same declaration
is already fine in a class), and one needs `nqp::defined` / `nqp::isconcrete` /
`nqp::unless` / `nqp::list` / `nqp::unshift`
([#8024](https://github.com/tokuhirom/mutsu/issues/8024)).

The behaviour is pinned by `t/modules/slang-package-declarator.t` against
`t/lib/SlangDeclarator.rakumod`, a fixture written in the same NQP idiom with a
class-kind declarator (`widget`, metaclass from the `EXPORT` body) and a
role-kind one (`gadget`, metaclass swapped inside the candidate) — the two
shapes `test-hub` and `test-bundle` use. The fixture is valid Raku: the test
passes 11/11 under rakudo as well as under mutsu, so the reading is
oracle-checked rather than asserted. `t/modules/import-export/use-from-nqp-is-a-noop.t`
pins the `:from<NQP>` half.
