# ADR-0081: A unit module's imported aliases are scoped to its compilation unit

- Status: Proposed
- Date: 2026-09-09
- Related: [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (a mainline
  routine carries the lexical store of its compilation unit),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (replace by-name
  lookup with an owning lexical scope), and
  [ADR-0047](0047-type-identity-is-a-declaration-site-not-a-registry-name.md)
  (a type binding is distinct from the type's registry identity)
- Addresses: GitHub issue [#7692](https://github.com/tokuhirom/mutsu/issues/7692)

## 1. Context

### 1.1 The observable bug

When a `unit module` imports a variable and a type for its own routines, mutsu
leaves the imported aliases in the caller's live environment. A script that
uses only the outer module can therefore see symbols that Rakudo keeps private
to the nested module:

```raku
# lib/L7Var.rakumod
unit module L7Var;
our $var-x is export = 42;
class ClsZ is export { method hi() { 'hi' } }
sub op-user() is export { 1 }

# lib/L7Mid.rakumod
unit module L7Mid;
use L7Var;
sub mid-p() is export { $var-x ~ '/' ~ ClsZ.new.hi }
```

```raku
use lib 'lib';
use L7Mid;
say mid-p();                         # 42/hi in both implementations
say $var-x;                          # Rakudo: undeclared
say ClsZ.new.hi;                     # Rakudo: undeclared
say op-user();                       # not declared in either implementation
```

The routine import is already scoped by the `module_registered_functions`
delta and the compunit visibility work from #7612/#7698. The remaining leak is
specific to imported variables and type aliases.

### 1.2 Why the existing tables do not solve it

`load_module_inner` runs a unit module's body in the caller's `env`, with the
runtime package still `GLOBAL`. While the body runs, a nested `use L7Var`
writes `$var-x` and `ClsZ` into that shared environment. The loader already
records the additions in two tables when the body finishes:

- `module_scope_lexicals[L7Mid]` retains the imported value for variable and
  bareword lookup after the loading frame is gone;
- `package_type_aliases[L7Mid]` retains the short `ClsZ` → `L7Var::ClsZ`
  binding for type lookup.

Those tables are currently only fallbacks. The newly imported aliases are not
removed from `env`, so the using scope wins before either fallback is reached.
Removing the aliases without fixing the read side would break `mid-p`: the
routine's frame has `package == GLOBAL` and no `lexical_package`, so
`lookup_in_running_package` cannot find the entries keyed by `L7Mid`.

This is why the issue is not fixed by another `pop_import_scope` retention
rule. The module's own imports must survive the load, but their visibility must
be tied to the module's compunit rather than to the caller's environment.

## 2. Decision

Keep the existing package-keyed scope tables, and give every executable frame
the owning package of the compunit whose lexical aliases it can see. Do not
make `env` the authority for a module's imported aliases.

The implementation is phased as one compatibility slice because either half
alone is observably wrong:

1. During a module load, save the prior values of names that the module body
   imports into the caller environment. After the module body has been
   recorded in `module_scope_lexicals` / `package_type_aliases`, remove the
   module-owned aliases and restore any caller values. This cleanup applies to
   aliases installed while the module body runs, not to the direct import that
   `use Module` performs after the module load returns. A direct `use L7Var`
   must continue to make its exported names available to the using scope.
2. Record the unit package (`L7Mid`) as lexical-package metadata for routines
   declared in that unit module. Propagate the metadata to the compiled
   function and all routine/block frame entry paths, including fast/light
   dispatch and closures. An inlined block with no own declaration file
   inherits its caller's lexical package.
3. Make package-keyed variable/type lookup probe the frame's lexical package
   first, then retain the existing method-class, routine-package, and current-
   package candidates. The existing package-chain walk remains the mechanism
   for `L7Mid` and any enclosing package. The same lexical-package field must
   participate in any context-sensitive resolution cache key.

With this shape, `mid-p` resolves `$var-x` through
`module_scope_lexicals[L7Mid]` and `ClsZ` through
`package_type_aliases[L7Mid]`, while the using script has no bare `env` alias
to resolve. A direct import still installs its own aliases after the module
load, so it remains visible where Raku exports it.

## 3. Invariants

The implementation must preserve these invariants:

- An import made by a module body is visible to that module's own routines,
  methods, nested closures, and EVALs lexically nested in those routines.
- A transitive import is not visible as a bare variable or type in the outer
  using scope. A routine exported by the outer module remains callable.
- A direct `use` or `require` in the using scope keeps its documented exported
  variables and types; cleanup of a module body must not remove the later
  direct-import aliases.
- A caller's same-named lexical is restored unchanged after module loading;
  module initialization must not overwrite it merely because both names share
  the temporary execution environment.
- Package-qualified access (`L7Var::var-x`, `L7Var::ClsZ`) and exported
  aliases retain their current behavior. This ADR changes the unqualified
  lexical visibility of a transitive import, not package identity or export
  tags.
- Ordinary package blocks and class methods retain their current lexical
  package behavior. `unit module` top-level routines are the missing case.
- Routine resolution, prelude visibility, variable lookup, and type lookup must
  agree on the same declaring compunit. A fix that makes only one of the three
  paths use `L7Mid` is incomplete.

## 4. Acceptance

Add a focused TAP regression under `t/` with two library modules and assertions
against both `raku` and mutsu. It must cover:

1. `mid-p` can read the nested exported variable and instantiate the nested
   exported class.
2. The outer using scope cannot compile/evaluate the nested variable or class
   by bare name.
3. The nested exported sub remains unavailable in the outer scope, preserving
   the already-correct routine direction.
4. A direct `use L7Var` still imports the exported variable and class.
5. A same-named caller variable is restored after loading `L7Mid`.
6. The existing module-scope regressions remain green:
   `t/block-use-keeps-nested-module-imports.t`,
   `t/module-reuse-class-in-block.t`,
   `t/nested-module-native-prelude-not-visible-to-user.t`,
   `roast/S11-modules/lexical.t`, and `roast/S11-modules/require.t` test 10.

The implementation PR must also run the normal `make test` and `make roast`
gates. The issue's repro must be checked against the installed Rakudo oracle,
not inferred from mutsu's current output.

## 5. Alternatives rejected

### 5.1 Leave the aliases in `env` and suppress only the outer lookup

Rejected. The live environment has no reliable provenance for whether a name
was installed by a direct import, a nested module import, or the caller's own
declaration. Scope predicates layered over the flat map would recreate the
same race between module loading and block/EVAL restoration that
`module_registered_functions` was introduced to avoid.

### 5.2 Re-key the scope tables by source-file symbol

Rejected for this slice. The existing package key is already the owner used by
class methods, package-chain lookup, distribution metadata, and re-import
repair. Re-keying by source file would duplicate the package-to-compunit
mapping and require every existing owner-based reader to learn a second key.
The missing information is the frame's lexical owner, not a different storage
key.

### 5.3 Change a unit module's runtime package from `GLOBAL` to its module name

Rejected. The `GLOBAL` registration is part of the current module export and
import machinery, and changing it would affect export aliases, `MAIN`, package
stash visibility, and nested module loading. Lexical ownership must be carried
as separate metadata, as it already is for method frames.

### 5.4 Delete the saved module-scope tables and rely on compiled captures

Rejected. The module body can be required from a method or otherwise run after
the caller's environment has disappeared. The existing tables are precisely
the durable store for these aliases, and type aliases cannot be safely reduced
to an ordinary copied value because the type registry identity is separate
from its short name (ADR-0047).

## 6. Implementation notes and open checks

The likely implementation touch points are:

- `src/runtime/run_modules.rs`: record the unit-package/source association,
  snapshot and restore module-owned `env` aliases, and keep direct imports
  outside the cleanup window;
- `src/ast.rs` / `src/opcode.rs` and the routine construction paths: carry the
  lexical package from a unit module declaration into compiled routines and
  closures;
- `src/runtime/accessors_stack.rs` and the four compiled-call paths: populate
  `RoutineFrame::lexical_package`, inheriting it for anonymous/inlined blocks;
- `src/runtime/types/type_registry.rs` and `src/vm/vm_env_helpers.rs`: probe
  the lexical package before the existing package candidates;
- `src/vm/vm_var_get_ops.rs` and `src/vm/vm_exec_dispatch.rs`: verify that
  variable and bareword fallback paths all reach the same owner-aware lookup.

Before accepting the implementation, re-check these less common paths:

- `require` from inside a method, where the loading caller frame disappears;
- an EVAL nested in an imported module routine;
- a closure handed to a native callback from that routine;
- two unrelated modules importing the same short type name;
- a block-scoped `use` followed by a repeated `use` after the module is already
  loaded.

This ADR intentionally does not change the broader `@`/`%` slot campaign in
ADR-0039, nor the identity model for lexical type declarations in ADR-0047.
Those mechanisms are prerequisites and neighboring constraints, not alternate
owners of this issue.
