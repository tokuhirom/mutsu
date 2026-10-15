# `declares_method` works on a class HOW that has a role mixed in

`Metamodel::MethodContainer.declares_method` decided whether its receiver HOW was a class HOW by
looking at the HOW value's class name, and a HOW with a role mixed in (`$how does Role`) is a
`Mixin` over the HOW instance, not an instance: the probe answered
`No such method 'declares_method' for invocant of type 'Mu'`. It now looks through the mixin.

AttrX::Mooish does exactly this to the HOW of a class with a mooish attribute and calls
`self.declares_method(type, $alias)` from the role method it mixes in (#13264, `t/080-aliasing`).

Pinned in `t/oo/method/declares-method-on-mixed-in-how.t`.
