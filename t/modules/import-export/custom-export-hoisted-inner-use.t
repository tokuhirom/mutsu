use Test;

# A sub hoisted to the head of a nested package block must still resolve a
# symbol a custom `sub EXPORT` installs via that same block's own `use`
# (#8564). mutsu hoists every `SubDecl` in a block so forward references
# resolve, which runs the hoisted sub's registration BEFORE the block's own
# in-position `use` statement executes; a custom `sub EXPORT`'s installed
# symbol lives only in `env` (unlike a tag-based `is export` routine, which
# also gets a registry entry), so it must survive the module's BEGIN-time
# preload instead of only reappearing once the in-position `use` re-runs
# EXPORT. JSON::Fast's `use JSON::Fast; sub render(...) is export { ...
# to-json(...) ... }` shape (t/routines/call/tail-stmt-call-named-value.t)
# is the real-world case this regressed.
plan 1;

use lib 't/lib/CustomExportHoistedInnerUse';
use Outer;

is caller-fn(), 'from inner custom EXPORT',
    'a sub hoisted ahead of its own block\'s use resolves the custom-EXPORT symbol';
