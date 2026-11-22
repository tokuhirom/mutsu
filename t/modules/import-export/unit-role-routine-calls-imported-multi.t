use lib 't/lib';
use Test;

# A `unit role` file's own routines call a multi the file's `use` imported:
# the role body is where they are declared, and it sees the file's imports
# (#17962; Red's `unit role Red::Driver::Cache`).

plan 3;

use UnitRoleImportMulti::ProtoRole;
use UnitRoleImportMulti::SubRole;

is qc('x'), 'db x', 'a multi of the role file calls an imported proto family';
is qe('x'), 'db x', 'a plain sub of the role file does too';
is qp('x'), 'plain x', 'and an imported multi with no proto of its own';
