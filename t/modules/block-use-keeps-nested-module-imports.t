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
# The opposite edge -- that the nested module's import was also visible to the
# USING scope, where rakudo hides it -- was GH #7612, which this file used to
# pin for the native provider's prelude splice. That provider is gone: NativeCall
# is the real upstream module now, and its `our sub`/`our proto` exports leak
# through a symbolic `::('&name')` lookup like any packaged multi export does
# (#12161), so the assertion is dropped here until that is fixed.
# `t/nativecall/nested-module-native-prelude-not-visible-to-user.t` still covers
# the shapes that hold.
use lib 't/lib';
use Test;

plan 5;

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

# And the nested module's own import stays invisible to the using scope.
{
    use BlockUseNestedMultiOuter;
}
my $nested-multi = '&nested-' ~ 'mexp';
nok defined(::($nested-multi)), "a nested module's multi import is not visible to the user";
