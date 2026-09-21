use Test;
use lib 't/lib';
use RolePrivateSubHelper;

plan 1;

# A plain (non-`is export`) top-level sub in a role body is a lexical helper
# of the role's own compilation unit, exactly like a module's own private
# top-level sub -- it must be callable from an exported sub of the same role
# without ever composing the role onto a class (`does`/`is`). This is the
# shape distributions like Date::Calendar::Strftime use for their exported
# `strftime` calling a private `_strftime` helper.
is greet('world'), 'hello, world',
    'an exported role sub can forward-reference a private role sub with no composition';

done-testing;
