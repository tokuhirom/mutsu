# A `use` inside a nested block loads at BEGIN time

Raku performs `use Foo` at BEGIN time: by the time any of the compunit's
mainline runs, every package `Foo` contributes is already installed. mutsu
compiles `use` to a runtime `OpCode::UseModule`, so a `use` written inside a
block that has not executed yet was invisible to code that *ran* earlier — even
when the `use` appeared earlier in **file** order. And because mutsu fabricates a
stub type object for an unresolved `::`-qualified bareword rather than erroring,
the divergence was silent: the reference answered the *written* name instead of
the module-composed one.

```raku
use lib 'tmp';
my &later = { use UseTypeFixture; };   # earlier in the file, not run yet
say X::Fixture::Marker.^name;
# raku:  UseTypeFixture::X::Fixture::Marker
# mutsu: X::Fixture::Marker              (a stub)
```

## What changed

`use` is now split into the two halves Raku actually has. The *load* is hoisted
to the head of the compilation unit; the *import* stays exactly where it was,
because Raku genuinely scopes a nested `use`'s imports to the block holding it
(`{ use Foo } foo()` must still die — `roast/S11-modules/lexical.t`).

The unit compile records every module `use`d anywhere inside it — including
inside closure and sub bodies, which share the unit's context the same way the
constant-fold state does. Subtracting the unit's own top-level `use`s leaves the
nested ones, and the unit is recompiled with an `OpCode::PreloadModule` for each
at its head. Only a unit that actually holds a nested `use` pays for that second
pass; it rides the pass the refold path already had.

`Interpreter::preload_module` is the load half: `use_module_with_tags` with the
final `import_module` skipped. `need_module` was not a substitute — it loads with
`suppress_exports` set, so the module's `is export` routines are never registered
and the later in-position `use` of the (now already-loaded) module has nothing
left to import.

Three details make hoisting the load safe:

- **`use lib` is replayed first.** A `use lib` written *later* in the file is a
  BEGIN-time effect too, and a hoisted load may depend on it, so the unit's
  literal `use lib` specs are emitted ahead of the preloads in source order.
  Adding the spec that is already at the front of the search chain is now a
  no-op, so the `use lib` at its own position recognizes the prologue's work
  instead of doubling the entry. A path merely present *deeper* in the chain is
  still promoted — `use lib` outranks `-I` and `MUTSULIB`
  (`t/modules/compunit/lib-path-precedence.t`).
- **The load runs inside a preload scope.** mutsu's sub hoisting installs every
  exported routine under `GLOBAL::` while a module body loads, and those aliases
  belong to whoever wrote the `use`. Until now they were contained by the import
  scope of the very block holding it; hoisting the load out would have left an
  exported `proto sub head(|)` shadowing the core listop for the whole file. The
  preload scope rolls the routine and proto registries back the same way an
  import scope does, but leaves the class registry alone — a package the module
  declares is exactly what the preload exists to publish, and raku installs it
  into GLOBAL at load time too.
- **A preload that cannot find its module is discarded.** Raku rejects an
  unresolvable `use` at BEGIN time whatever block it sits in; mutsu deliberately
  does not. Hoisting must not change that, so the preload swallows its own
  failure and the in-position `UseModule` still reports it if control reaches it.

## What it unblocks

`JSON::Tiny`'s upstream `t/01-parse.t` is back on `batteries-whitelist.txt` at
93/93. Its last assertion is

```raku
throws-like {
    use JSON::Tiny;
    from-json '',
}, X::JSON::Tiny::Invalid;
```

and `throws-like`'s two arguments both evaluate before the block is invoked, so
the type reference was read before the block's `use` had run. mutsu handed
`throws-like` a stub named `X::JSON::Tiny::Invalid` while the block threw the
module's real `JSON::Tiny::X::JSON::Tiny::Invalid`, and 92 of the file's 93
assertions passed. That assertion used to "pass" only because both sides were
mutsu fabrications agreeing with each other; retiring the native JSON
interception (#8183) stopped hiding the real gap.

## Residual

A top-level `use` still loads where it stands, not at the head of the unit, so
code textually *before* it in the mainline still cannot see its packages. That is
the same BEGIN-time gap one level up, but it needs no stub fabrication to be
visible and is much rarer in practice; hoisting every top-level load ahead of the
mainline is a considerably larger change in blast radius than this one.

Pin: `t/modules/import-export/use-in-nested-block-is-begin-time.t`.
Closes [#8201](https://github.com/tokuhirom/mutsu/issues/8201).
