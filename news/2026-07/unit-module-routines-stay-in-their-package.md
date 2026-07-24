# A `unit module`'s routines stay in its own package

Until now mutsu had essentially no scoping for routines declared in a
`unit module Foo;` file. The declaration switched the *compiler's*
`current_package` but not the runtime's, and routine registration is keyed off
the runtime package — so every `sub` in such a module landed in `GLOBAL::` and
became callable by its bare name from anywhere that merely `use`d the module,
regardless of `is export` and regardless of `my` / `our` / no scope declarator.

```raku
# lib/OurSub2.rakumod
unit module OurSub2;
sub plainhelper($a) { ... }        # not exported
our sub ourhelper($a) { ... }      # `our`, still not exported
my sub myhelper($a)  { ... }       # explicitly lexical
```

```raku
use OurSub2;
plainhelper('x');   # raku: "Undeclared routine"   mutsu (before): called it
```

This was more than a tidiness problem: a leaked routine could shadow a builtin
at a call site that never asked for it. `roast`'s `Test/Util.rakumod` declares a
non-exported `our sub run(Str, Str, *%o)`, which was a live candidate for every
plain `run(...)` in a file that `use`s `Test::Util`. It also blocked a known
correctness fix to positional-parameter indexing, which would have let that
leaked `run` start winning over the core builtin (PLAN 8.22).

## What changed

The runtime package now follows the compiler's. A new `SetCurrentPackage`
opcode is emitted for a `unit module` / `unit package` declaration, and — this
is the part that matters — it is emitted **before the sub-hoist pass**, not
where the declaration itself sits. Sub declarations are hoisted to the top of a
compilation unit so `&name` references work ahead of the declaration; hoisting
them while the runtime package was still `GLOBAL` installed a second,
bare-named copy of every routine, which is exactly the leak.

Several further package-blindness bugs surfaced once registration became
package-scoped, all of which had been masked by everything living in `GLOBAL`:

- The name-keyed OTF body cache (`otf_call_cache`) was package-blind in both
  directions. It now records the package the resolution was made under, so an
  entry is only reused from that package, and it carries the routine's
  *defining* package so a cache hit runs the body under the same package a
  fresh resolve does. The latter keys `__mutsu_callable_id::PKG::NAME`, which
  in turn keys `once` — without it, a `once` inside a module sub fired again on
  the second call, because the first call ran under the module's package and
  the cached second call under the caller's.
- `has_proto` consults the `proto_subs` name set, so importing a module's
  exported `proto` had to record it there under the importing package too.
- A routine body compiled on its own has no parameter locals, so the compiler
  qualifies its bare variable reads with the routine's package. `call_function`
  compensates by aliasing each bound parameter under that qualified name;
  `exec_call` — the statement-call path a `is test-assertion` routine takes —
  did not, so an `@`/`%` parameter read as empty inside it. Both now share one
  helper. (`Test::Assuming`'s `is-primed-call` saw `@expect` as `[]`.)
- END phasers run at program exit, with `current_package` back at GLOBAL. An
  END declared in a `unit module` now records and restores its declaring
  package, so it still resolves that module's own routines by their bare names
  (roast's `Test::Compile` has an END calling its module-private
  `delete_compunits`).
- `require "file"` and `CompUnit::Repository::FileSystem.need` ran the loaded
  file's mainline under whatever package the *caller* happened to be in. A
  compilation unit is not a continuation of the requiring scope: both now run
  it under GLOBAL, like `load_module` already did. (`Test::Compile.do_compunit`
  runs in `Test::Compile`, which was registering a compunit's
  `package Pod { class Ber {} }` as `Test::Compile::Pod::Ber`.)
- Writing an `our` variable through its lexical alias (`our $x; ...; $x = 7`)
  only reached the package variable at block exit, so `$Foo::x` read `Nil` —
  and a module mainline never "exits" before its consumer runs, which made
  every exported `our` variable read as `Nil`. `SetLocal` into an `our`-linked
  slot now pushes the value out to the package variable immediately, the mirror
  of the already-existing qualified→lexical sync. This one was a pre-existing
  bug in its own right: `module Foo { our $x; $x = 7 }; say $Foo::x` printed
  `Nil` instead of `7`. Pinned by `t/our-var-package-write.t`.

Reading the package identity per call had to be cheap, so `current_package`
gained an interned-`Symbol` mirror behind a relaxed atomic; the existing
`RwLock<String>` accessor clones a `String`, which is far too expensive for the
hot dispatch path.

## `roast/S11-modules/re-export.t` leaves the whitelist

`OuterModule` re-exports via `use InnerModule :ALL, :EXPORT`, and rakudo
rejects `:EXPORT` as an undeclared tag — **the file cannot pass upstream
either**. mutsu only appeared to pass it because Inner's subs leaked into
`GLOBAL::` and the consumer saw them without any re-export happening. It is
recorded in `TODO_roast/BLOCKERS.md` with the raku baseline.

## `use MONKEY-GUTS` is a recognized pragma

Found while investigating the fallout: `MONKEY-GUTS` is a core Rakudo pragma
(it only lifts the ban on `nqp::` ops, which mutsu does not enforce), but mutsu
did not recognize it, so `use MONKEY-GUTS` failed. Because a failed `use`
aborts the rest of the module's mainline, every declaration after line 5 of
roast's `Test::Util` — including all seven of its `proto ... is export`
declarations — silently never ran. The module appeared to work only because
sub hoisting had already registered its `sub`s. It is now recognized alongside
`MONKEY-TYPING` / `MONKEY-SEE-NO-EVAL` / `MONKEY`.

Pinned by `t/unit-module-scope.t`, which passes identically under `raku`.
