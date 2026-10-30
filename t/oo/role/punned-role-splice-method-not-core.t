use Test;

plan 4;

# `Role.new` puns the role into an instance whose class is the role. A `splice`
# the role (or a role it composes) declares answers the call, not the core
# `Any.splice` "Routine does not have any candidates" refusal (Dan's `Series`).
role Base {
    has @.d = 1, 2, 3;
    method splice($start = 0, $elems?, *@replace) { "base splice({ $start ~~ Callable ?? "code" !! $start })" }
}
role Derived does Base { }
class Concrete does Derived { }

is Base.new.splice(1), 'base splice(1)', 'a punned role with its own splice';
is Derived.new.splice(2, 1), 'base splice(2)', 'a punned role composing the role';
is Derived.new.splice(*-1), 'base splice(code)', 'a WhateverCode start binds';
is Concrete.new.splice(0), 'base splice(0)', 'a class composing the role';
