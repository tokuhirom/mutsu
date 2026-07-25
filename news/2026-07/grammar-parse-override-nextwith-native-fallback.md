# A grammar `parse`/`subparse` override can `nextwith` to the native parse

A grammar that overrides `parse` (or `subparse`/`parsefile`) to wrap the native
grammar parse — the common pattern for injecting an `:actions` object —

```raku
grammar G {
    token TOP { ... }
    class Actions { ... }
    method parse($input, *%args) {
        nextwith($input, :actions(Actions), |%args);
    }
}
```

now works. Previously the `nextwith`/`nextsame` (and `callwith`/`callsame`)
inside the override had no MRO candidate to defer to: the built-in grammar parse
is not a `MethodDef`, so it never appears in the dispatch candidate list, and the
re-dispatch fell through to `Value::NIL` — every `G.parse(...)` returned an
undefined match. This is the shape YAMLish's `load-yaml` relies on
(`method parse { nextwith($input, :actions(Actions)) }`).

Two fixes were needed:

1. **Native grammar parse as the final `nextsame`/`nextwith` candidate.**
   `dispatch_next_candidate` now falls through to `dispatch_package_parse` when
   an overridden grammar `parse`/`subparse`/`parsefile` exhausts its user MRO —
   mirroring the existing metamodel-HOW native fallback. A frame is pushed for
   such an override even with a single user candidate
   (`push_method_dispatch_frame` / `run_instance_method_celled`) so the deferral
   has somewhere to land.

2. **A module-local `grammar Grammar` is recognised as a grammar.** The parser
   used to drop the default `Grammar` superclass for any declaration literally
   named `Grammar`, to avoid a self-parent. But inside a module a
   `grammar Grammar` qualifies to `Mod::Grammar` — a distinct type that should
   still inherit the built-in `Grammar`. Without the parent, `class_is_grammar`
   returned false for `Mod::Grammar`, so the native-parse fallback above never
   fired for it (YAMLish declares exactly `grammar Grammar` inside
   `unit module YAMLish`). The parser now always adds the `Grammar` default
   parent, and a self-parent (a genuine top-level `grammar Grammar`, or any
   `class Foo is Foo`) is filtered out at registration in
   `exec_register_class_op`.

Pins: `t/grammar-parse-override-nextwith.t`,
`t/grammar-named-grammar-in-module.t`.
