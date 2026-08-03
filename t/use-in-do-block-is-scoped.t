# An import is lexical to the block that asked for it, and a `do { }` block is a
# block: `my (&f) = do { use M; (&f) }` takes the routines it names as values
# and leaves everything else M exports out of the enclosing scope. Two halves
# were missing:
#   * `do { }` never emitted PushImportScope/PopImportScope at all (only the
#     statement-form bare block did), so every import leaked.
#   * `pop_import_scope` rolled back `functions`/`classes` but not the two proto
#     tables, so an imported `proto sub head(|)` stayed visible to `has_proto`
#     and kept a later bare `head(3, @a)` on the user-routine argument path
#     (VarRef-wrapped array) instead of the core listop's flattened one.
# roast/S32-list/skip.t is the file this was found on: it imports `Test`
# selectively through exactly this shape so the core `skip` stays reachable.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 7;

my @array = <a b c d e f g h>;

my (&taken) = do {
    use ScopedProtoExport;
    is scoped-only(), "in scope", "an imported sub is visible inside the do block";
    is head("x"), "module-head:x", "an imported proto beats the core listop inside the block";
    (&scoped-only)
};

is taken(), "in scope", "a routine taken out of the block as a value still works";
is head(3, @array).join(","), "a,b,c", "the core listop is back after the do block";
dies-ok { EVAL 'scoped-only()' }, "the imported sub is out of scope after the do block";

# The statement-form bare block scopes the proto too.
{
    use ScopedProtoExport;
    is head("y"), "module-head:y", "an imported proto is visible inside a bare block";
}
is head(3, @array).join(","), "a,b,c", "the core listop is back after a bare block";
