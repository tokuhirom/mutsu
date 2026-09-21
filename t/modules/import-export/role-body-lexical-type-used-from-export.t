use Test;
use lib 't/lib';
use RoleLexicalTypeHelper;

plan 1;

# A `my grammar`/`my class` declared directly in a role body is a lexical
# declaration of the role's own compilation unit -- it must be usable from
# an exported sub of the same role with no class ever composing the role.
# This is the shape Date::Calendar::Strftime uses: its private `_strftime`
# helper parses with a `my grammar prt-format {...}` and builds results
# with a `my class re-format {...}`, both declared in the role body
# alongside it.
is greet('world'), 'hello, world',
    'an exported role sub can use a lexical grammar/class declared in the same role body with no composition';

done-testing;
