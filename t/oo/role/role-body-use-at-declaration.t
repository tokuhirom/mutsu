# A `use` inside a role body is BEGIN-time: what it imports is in scope for the
# role's own declaration (its `also does` parent, a nested role's parent, an
# attribute trait), even when the role itself is never composed. Found via
# PDF::Class (PDF::Destination, PDF::Attributes::Layout).
use Test;
use lib 't/lib';

plan 7;

use RoleBodyUse::Dest :DestDict;
use RoleBodyUse::Layout;

class C does RoleBodyUse::Dest { }
is C.new.tied, 'tied', 'also does a role from a module used in the role body';

class D does DestDict { }
is D.new.tied, 'tied', 'a nested my role does a role from the outer body use';
is D.new(:page(3)).pg, 'alias of page: 3',
    'the outer body use supplies the nested role attribute trait';

class L does RoleBodyUse::Layout { }
is L.new.common, 'common', 'a unit role does a my role declared in its own body';
is L.new.fit-horiz, 'horiz', 'a body enum member constrains a role method parameter';

role Inline {
    use RoleBodyUse::Tie;
    my role Inner does RoleBodyUse::Tie { }
    method inner { Inner }
}
class E does Inline { }
is E.new.inner.^name, 'Inline::Inner', 'nested role in an inline role';
ok E.new.inner ~~ RoleBodyUse::Tie, 'nested role composed the used role';
