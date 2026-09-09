# A transitively-`use`d module's package name no longer leaks into the importer

`use Outer;` — where `Outer.rakumod` itself begins `use Inner;` — left the bare
name `Inner` resolvable in the importing file, which never asked for it and
where rakudo reports an "Undeclared name":

```raku
# lib/Inner.rakumod:  unit module Inner; class InnerClass is export { }
# lib/Outer.rakumod:  use Inner; unit module Outer; class OuterClass is export { }

use Outer;
OuterClass.new;                # correct: Outer exported it
Inner::InnerClass.new;         # rakudo: Could not find symbol.  mutsu: resolved
```

This is the last piece of issue #7555 item 1's divergence (b). The alias half of
that divergence — the *classes and roles* a transitively-loaded module declares,
and the variables and subs it exports — was closed by #7743 (issue #7692), which
scopes a module's imports to its own compunit. Measured against that fix, one
binding still escaped: the module's own package name.

## Root cause

`OpCode::RegisterPackage` binds a `unit module X` under its bare name with
`env.insert`, and a module body runs in the **caller's** env
(`load_module_inner` → `run_block`) rather than in an env of its own. So when
`Outer`'s body executes its `use Inner;`, the `Inner` package binding lands in
whatever frame triggered the load — the importing file, for a compile-time `use`
at file scope. #7743's cleanup restores the caller's plain bindings and undoes
the aliases `import_module` recorded, but a package name is neither: it is a new
env key nobody registered as an import.

It is now dropped at the end of the load that introduced it, unless it names the
module being loaded or something nested under it. A direct `use Inner;` is
unaffected — that load keeps its own package name, exactly as rakudo puts it in
the importing scope's `MY::`.

The registry diff is consulted alongside the name test, because a module file
with no `unit` declarator can register a class under a name unrelated to its own:
`roast/packages/RT128156/lib/RT128156/Top1.rakumod` declares a plain
`class Top1`, which roast's `S10-packages/precompilation.t` requires to be
visible in the loading scope's `MY::`. A name test alone drops it and fails that
test.

Pinned by `t/module-transitive-use-does-not-leak-types.t`, which passes 9/9
verbatim under rakudo v2026.07 as well as under mutsu, and covers the parts
#7743 fixed as well as this one — including the two-hop case (a module that
`use`s a module that `use`s a third), whose method bodies resolve bare names
through their own class's package chain rather than through `env`.

## What is still divergent

A module's `constant`s and enum values reach the importer the same way and are
deliberately left alone. Removing them needs somewhere else for the module's own
top-level subs to find them: `module_scope_lexicals` is keyed by package and a
module's plain `sub` runs under `GLOBAL`, so dropping the env binding turns
`Log::Async`'s `sub trace { ... :level(TRACE) ... }` into a bareword `Str`.
mutsu also keeps module package names in `GLOBAL::` where rakudo keeps them in
the importing compunit's `MY::`; the fix here scopes their *visibility*
correctly without moving that storage. Both belong to the module-lexical scoping
campaign #7555 describes.
