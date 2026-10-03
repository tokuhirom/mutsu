use Test;
use lib 't/lib';

plan 3;

# A parametric role's parameter defaults are written in the role's own
# scope: they name what its module imported (`Pretty`) and its package's
# classes (`Basic`), neither of which the composing scope can see.
use RoleParamDefaultScope::Tree;

class IntTree does RoleParamDefaultScope::Tree[Int] { }
is IntTree.new.kinds, 'Int RoleParamDefaultScope::Pretty RoleParamDefaultScope::Basic',
    'composition binds the defaults in the declaring scope';

is RoleParamDefaultScope::Tree.kinds,
    'Any RoleParamDefaultScope::Pretty RoleParamDefaultScope::Basic',
    'a pun binds them there too';

is RoleParamDefaultScope::Tree.new.kinds,
    'Any RoleParamDefaultScope::Pretty RoleParamDefaultScope::Basic',
    'and so does .new on the pun';
