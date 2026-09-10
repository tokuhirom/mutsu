# A module first `use`d inside a block must stay fully usable when a sibling
# block `use`s it again: `loaded_modules` is never rolled back, so the second
# `use` is a no-op — the module's own package-qualified classes must therefore
# survive the first block's import-scope pop (pop_import_scope), exactly like
# its package-qualified functions already do. Dropping them left method
# dispatch on the class dying with X::Method::NotFound in the second block.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 4;

{
    use ScanCacheHelper;
    is ScanCacheThing.new.label, "thing", "class instantiates in the first block";
}

{
    use ScanCacheHelper;
    is ScanCacheThing.new.label, "thing", "class instantiates after re-use in a sibling block";
    is ScanCacheThing.^name, "ScanCacheHelper::ScanCacheThing", "re-used class keeps its qualified name";
    is scan-cache-greet("again"), "hello, again", "exported sub still works after re-use";
}
