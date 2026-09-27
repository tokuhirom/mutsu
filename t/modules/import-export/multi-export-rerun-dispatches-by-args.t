use Test;
use lib 't/lib';

# Every `use` runs the module's EXPORT again with ITS arguments. A
# `multi sub EXPORT` must dispatch on them each time, not replay whichever
# candidate the first import picked (FINALIZER: a helper module's
# `use FINALIZER <class-only>` then the test's plain `use FINALIZER;`).
# EXPORT's own lexicals must survive the block that first loaded the module:
# its `my class Token` is constructed again on every import.

plan 4;

{
    use MultiExportByArgs 'tagged';
    is which-export(), 'tagged token', 'the first import dispatches on its argument';
}
{
    use MultiExportByArgs;
    is which-export(), 'no-args token', 'a later import without arguments gets the other candidate';
}
{
    use MultiExportByArgs 'tagged';
    is which-export(), 'tagged token', 'and back again';
}
use MultiExportByArgs;
is which-export(), 'no-args token', 'a top-level import after the block-scoped ones';
