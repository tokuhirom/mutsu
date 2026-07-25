# A user type named like a built-in resolves to the built-in inside a module

Blocker #2 for the `YAMLish` battery candidate (`docs/batteries/yaml.md`), now
**root-caused**.

Inside a `unit module`, a user-declared type whose name collides with a built-in
type name (here `grammar Grammar`, colliding with the core `Grammar` type)
resolves — when referenced by its **unqualified** bareword name from a sub in the
same module — to the **built-in** type object, not the module-local declaration.

## Isolated repro

`lib/GMod.rakumod`:

```raku
unit module GMod;
grammar Grammar {
    token TOP { \d+ }
}
our sub do-parse(Str $input) is export {
    say "Grammar is: ", Grammar.^name;      # raku: GMod::Grammar   mutsu: Grammar
    say "Grammar.HOW: ", Grammar.HOW.^name;  # raku: GrammarHOW      mutsu: ClassHOW
    return Grammar.parse($input);
}
```

```raku
use GMod;
say do-parse("123");
# raku:  ｢123｣
# mutsu: X::Method::NotFound: Unknown method value dispatch (fallback disabled): parse
```

Renaming the grammar to `MyGrammar` makes it work, and the same
`grammar Grammar {…}` in the **mainline** (no module) also works — the bug is
specifically **unqualified type-name resolution inside a module preferring a
built-in over a module-local declaration of the same name**.

## Mechanism

`Grammar` resolves to the built-in `Grammar` type object (`.HOW` = `ClassHOW`,
not `GrammarHOW`), so `.parse` is routed to `dispatch_classhow_method`
(`src/runtime/methods_classhow_dispatch.rs`) as a meta-method, falls through its
match, and raises `X::Method::NotFound: Unknown method value dispatch (fallback
disabled): parse` (the catch-all at ~line 1114). raku instead resolves `Grammar`
to `GMod::Grammar` (a `GrammarHOW`) whose inherited `.parse` runs.

The fix is in bareword type-name resolution: an unqualified name referenced
inside `module Foo` must prefer a lexically-visible / module-local (`Foo::Name`)
type/grammar/class declaration over a built-in type of the same name. This is a
general name-resolution ordering bug, not grammar-specific (a `class Int {…}`
inside a module would shadow the built-in `Int` the same way).

## Impact

`YAMLish` is `unit module YAMLish` and declares `grammar Grammar` (plus
`Schema::JSON` / `Schema::Core` / `Schema::Extra`); `load-yaml` calls
`Grammar.parse($input)` unqualified (lib/YAMLish.rakumod:944). With blockers #1
and #1.5 fixed the module now loads, and this is the next failure on the
`load-yaml` path. Further grammar-feature gaps may surface after it, since the
YAML grammar (lib/YAMLish.rakumod:150–783) is large and action-heavy.
