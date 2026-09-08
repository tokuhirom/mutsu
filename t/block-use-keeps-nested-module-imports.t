# A `use` inside a block must not drop a NESTED module's own imports.
#
# A `unit module`'s body runs at `current_package() == GLOBAL`, so a `use` in
# that body registers its aliases under `GLOBAL::`. `pop_import_scope` dropped
# every `GLOBAL::`-prefixed entry the scope had added, and `loaded_modules` is
# never rolled back -- so after `{ use Outer; }` the later top-level `use Outer`
# was a no-op that could not restore what the pop had taken, and the chain saw
# its own import as missing (GH #7580). The block twin of the EVAL bug fixed in
# `news/2026-09/module-loaded-in-an-eval-keeps-its-imports.md`.
#
# The constraint in the other direction is `roast/S11-modules/lexical.t`:
# `{ use Foo }` must still leave `foo()` unresolvable outside the block. That
# holds because the module-body delta is taken BEFORE `import_module`, so an
# alias installed for the IMPORTING scope is never in the retained set.
#
# NOTE: do NOT name the provider the fixtures import in a comment here. mutsu
# picks a provider module out of the source text even inside a comment, which
# loads it at the top level and masks the very failure this file pins (GH
# #7611). Delete this paragraph when that is fixed.
#
# The opposite edge -- that the nested module's import is also visible to the
# USING scope, where rakudo hides it -- is pre-existing and tracked as GH #7612;
# it reproduces with no block at all, so it is deliberately not asserted here.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 4;

{
    use BlockUseNestedOuter;
}
use BlockUseNestedOuter;
is outer-probe(), 'visible',
   "a nested module's own import survives an enclosing block's import-scope pop";

# The same shape one level down: the block-scoped `use` is of the module that
# does the nested import itself.
{
    use BlockUseNestedInner;
}
use BlockUseNestedInner;
is inner-probe(), 'visible',
   "the nested importer's own view of its import survives too";

# The constraint in the other direction, with a plain module: a block-scoped
# `use` must still not leak its own imported alias past the block. That alias is
# installed by `import_module` AFTER the module-body delta is taken, so it is
# not in the retained set.
{
    use BlockUseNestedLeaf;
}
nok defined(::('&leaf-probe')),
    "a block-scoped use does not leak the importing scope's own alias";

# And the chain still resolves after yet another block-scoped `use`.
{
    use BlockUseNestedInner;
}
is outer-probe(), 'visible', 'the chain still resolves after a repeated block-scoped use';
