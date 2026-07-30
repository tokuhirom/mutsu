# The parse-time module export scan is memoized per file path (see
# parser/stmt/simple/module_exports.rs). A cache hit must replay the same
# registrations a fresh scan performs: exported subs (parseable as listop
# calls), exported operators, declared type names, and enum values. The first
# `use` here is the scan miss; the ones in later blocks are cache hits, so
# every assertion after the first block exercises the replay path.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 8;

{
    use ScanCacheHelper;
    is scan-cache-greet("miss"), "hello, miss", "exported sub works on scan miss";
    is (1 scan-cat 2), "1|2", "exported operator works on scan miss";
    ok ScanCacheThing.new.label eq "thing", "declared class visible on scan miss";
    ok ScanRed ~~ ScanCacheColor, "enum value term parses on scan miss";
}

{
    use ScanCacheHelper;
    is scan-cache-greet("hit"), "hello, hit", "exported sub works on scan cache hit";
    is (3 scan-cat 4), "3|4", "exported operator works on scan cache hit";
    # `.new` here would trip a pre-existing re-`use` method-dispatch bug
    # (todo/tickets/reuse-in-block-class-method-dispatch.md); the type-term
    # parse is what the scan cache is responsible for.
    ok ScanCacheThing ~~ Mu, "declared class visible on scan cache hit";
    ok ScanBlue ~~ ScanCacheColor, "enum value term parses on scan cache hit";
}
