# Grammar actions fire for subrules inside an abandoned alternative

When a `||` alternative matched a subrule and then failed later, so the grammar
recovered through a fallback alternative, mutsu dropped the subrule's action
because the surviving match tree no longer contained it. Rakudo calls an action
the moment its rule reduces. mutsu now dispatches those actions too. Found with
the CSS::Grammar distribution, whose error-recovery warnings
(`skipping: ...`) are emitted by such an action; its `t/error-handling.t` now
passes.
