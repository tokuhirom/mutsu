# An `Attribute`'s reading metamethods are answered at the call opcode

`$attr.name`, `.package`, `.type`, `.get_value($obj)` and the other reading
metamethods of an `Attribute` from `.^attributes` were already answered from
`Attribute`'s method-table rows without the interpreter cascade, but only from
inside the compiled dispatch, after the call opcode's receiver probes and
argument handling: about 20k instructions for `$attr.package`. EEL's per-fork
state copier calls four of them for each attribute of every event it copies
(#17530).

A lane at the top of `CallMethod` and `CallMethodMut` now answers them for a
receiver that is exactly an `Attribute` instance (no subclass, no mixed-in
role, no `augment` declaring the method), when no argument is `|`-spread, a
container, a `Proxy` or a lazy list and nothing wraps a method or declares a
`^find_method`. The writing metamethods (`set_value`, `set_rw`, `set_build`)
keep the full path.

`.get_value($obj)` went from 24k to 10k instructions and `.package === T`
from 21k to 10k (release build, callgrind).
