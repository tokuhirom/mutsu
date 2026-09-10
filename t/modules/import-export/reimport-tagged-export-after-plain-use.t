use v6;
use lib 't/lib';
use Test;

# Regression (TODO_dist T-042, Math::Arrow): a `use Mod :tag` that follows a
# plain `use Mod` must still import the tag-only exports. A bare-file module
# registers its exports only under GLOBAL; the plain `use` hid the tag-only
# ones, and a later tagged `use` could not restore them (they were deleted
# rather than kept module-qualified). Math::Arrow does exactly this:
#   use Math::Arrow;            # plain
#   use Math::Arrow :constants; # then tag-only -> `&term:<G>` must be defined

use TaggedExportFixture;            # plain use: DEFAULT exports only

plan 4;

ok &always-here.defined, 'DEFAULT export imported by the plain use';

use TaggedExportFixture :extra;     # tagged use of the already-loaded module

ok &only-extra.defined, ':extra sub imported by the later tagged use';
is only-extra(), 'extra', 'the re-imported :extra sub is callable';
ok &term:<TAGTERM>.defined, ':extra term:<> sub imported by the later tagged use';
