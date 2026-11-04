use lib 't/lib';
use Test;

# Found via the Shareable distribution (Storable::Lite): an `is export` sub of
# a role body calls a private `sub` of the same body. When the role is
# composed into a class declared in ANOTHER compunit, the exported sub was
# re-declared in the composer's package and lost sight of the helper.
use RoleExportHelper::Shared;

plan 1;

class Foo is RoleExportHelper::Shared { has $.ID }

is Foo.new(:ID(1)).to-text, 'Foo.new(ID => 1)',
    'exported role sub reaches its private helper through a class from another unit';
