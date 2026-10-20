use Test;
use lib 't/lib';

# A `use` in one role candidate's body does not hide the operator another
# candidate of the same role exports, even when the used module exports a
# candidate of that operator (CSS::Units' `:ops` next to `use Color`, #14161).
plan 4;

use RoleBodyOps::Units :pt, :ops;

my $r = 1upt + 2upt;
is +$r, 3, 'the exported operator adds';
is $r.type, 'pt', 'the exported operator keeps the role';
is 1 + 2, 3, 'plain addition is unaffected';
is RoleBodyOps::Units.new.plain, 'plain', 'the candidate with the use still works';
