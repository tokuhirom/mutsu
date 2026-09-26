use lib 't/lib';
use Test;

# `also is` / `also does` whose types arrive from a `use` inside the class body
# (CSS::Module's Actions and grammar classes are all written this way):
#  - a role stub (`method dimension (|) { ... }`) composed by `also does` is
#    satisfied by a parent that the body's own `use` brought in. The composition
#    check used to run against the class published before the body, which
#    lacked every `also is` parent the body introduced.
#  - `also is Role` puns the role and keeps the pun in the MRO, as the header
#    `is Role` form does.
#  - a deferred `also is` parent keeps its source position relative to parents
#    that were already loaded.

use AlsoIsBodyComp::Early;

plan 5;

class Kid {
    use AlsoIsBodyComp::Base;
    use AlsoIsBodyComp::Act;
    also is AlsoIsBodyComp::Base;
    also is AlsoIsBodyComp::Act;
    use AlsoIsBodyComp::Ext;
    also does AlsoIsBodyComp::Ext;
}

is Kid.new.dimension(1), 'act-dimension', 'stub satisfied by an in-body-used class parent';
is Kid.new.number(1), 'base-number', 'stub satisfied by an in-body-used punned role parent';
is Kid.^mro.map(*.^name).join(' '), 'Kid AlsoIsBodyComp::Base AlsoIsBodyComp::Act Any Mu',
    'the punned role takes its place in the MRO';

class Ordered {
    use AlsoIsBodyComp::Act;
    also is AlsoIsBodyComp::Act;
    also is AlsoIsBodyComp::Early;
}
is Ordered.^mro.map(*.^name).join(' '), 'Ordered AlsoIsBodyComp::Act AlsoIsBodyComp::Early Any Mu',
    'a deferred parent keeps its source position before an already-loaded one';
is Ordered.new.early, 'early', 'both parents are inherited';
