use v6;
use Test;

plan 2;

# A plain default `use Mod` must NOT import a sub that is only exported under a
# non-DEFAULT tag. This is the leak-prevention direction that mutsu's
# `module_owned_exports` attribution preserves: `internal` is tagged
# `:internals`, so a tag-free `use TagExp` leaves it undeclared while the
# default-tagged `always` is imported.

use lib $?FILE.IO.parent.add('lib-tagged-export').Str;

use TagExp;

is always(), "always", 'a default-tagged export is imported by a plain `use`';

my $leaked = (try internal("nope")).defined;
nok $leaked, 'a :tag-only export is NOT imported by a plain `use`';
