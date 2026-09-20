# A class-body `use` import is now callable from that class's own methods

`use Some::Module;` written directly inside a class body imports the
module's exported subs into that class's own package, as expected. But a
**method** of that same class could not call the imported sub: mutsu threw
`Unknown function: <name>` even though the class-body-level statements
themselves resolved it fine.

The root cause was in method dispatch's `current_package` anchoring
(`vm_method_dispatch.rs`): it only switched `current_package` to the
receiver's class during a method call when the class had its own directly
declared `sub`s (`has_class_scoped_subs`, tracked by `run_class_body`'s
per-statement walk), package-scoped `my` statics, a namespaced (`::`) name,
or a role's deferred-body import. A plain, flat class whose only
class-scoped routine came from an ordinary `use` statement in its own body
matched none of those cases, so `current_package` stayed wherever the
enclosing scope left it (typically `GLOBAL`) for the whole method call, and
`bare_name_packages()` never looked at the class's own package — where the
imported sub was actually registered (`import_module` targets
`current_package()`, which is the class while its body runs).

`run_class_body` (`registration_class_body.rs`) now also treats a
class-body `use`/`import` statement as installing class-scoped routines:
after such a statement runs, any function newly registered under the
class's own package is recorded the same way an explicitly declared `sub`
already was, so `has_class_scoped_subs` sees it and method dispatch anchors
`current_package` to the class as expected.

This was found via the ecosystem distribution roulette in
`Karabiner::CompModGenerator`, whose `Karabiner::Template` class does
`unit class Template; use Template::Classic; method rule_generator($tmpl) {
... template(...) ... }`.

Regression test: `t/modules/class-body-use-import-visible-in-method.t`.

Closes [#8883](https://github.com/tokuhirom/mutsu/issues/8883).
