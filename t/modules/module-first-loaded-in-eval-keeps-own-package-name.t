use v6;
use Test;

# #7764 taught the module loader to drop a `Package`-valued env binding that
# isn't owned by the module being loaded and wasn't a class/role it just
# registered (see `leaked_packages` in src/runtime/run_modules.rs). That is
# correct for a binding LEAKED from a transitive `use`, but the bookkeeping
# meant to put a module's own bindings back after a scope loses them
# (`module_package_globals` / `reinstate_module_package_globals`) only ever
# recorded `::`-qualified keys -- never the module's OWN bare package-name
# binding (a `unit module Foo;` binds bare "Foo" in env).
#
# So when a module's FIRST load happens inside a nested call frame whose own
# env overlay is discarded on return (a sub wrapping an EVAL, exactly the
# shape of Test's own `use-ok`: `EVAL ( "use $code" )` inside `try { }` inside
# `multi sub use-ok`), the module's bare package-name binding vanished with
# that frame -- while `loaded_modules` kept it recorded as loaded. A later
# real `use` of the same module is then a no-op (it is already "loaded") that
# never re-installs the missing binding, so the module's own name became
# permanently unresolvable. See
# https://github.com/tokuhirom/mutsu/issues/7806 (the "NativeLibs: MISSING"
# row, reproduced there via `Test`'s real `use-ok` + a second `use NativeLibs`).

plan 1;

use lib 't/lib/Issue7806';
use Issue7806Loader;

issue7806-use-ok('Issue7806Own');
use Issue7806Own;

ok ::('Issue7806Own') !~~ Failure,
    "a module's own package name is resolvable after use-ok-style first load "
    ~ "inside an EVAL, followed by a real (now-a-no-op) re-`use`";
