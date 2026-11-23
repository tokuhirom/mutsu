# A planned method whose body calls something runs in the lean frame

Calls such as `$s.chars`, `($r * 4).Int` and `self.other(...)` used to keep a
planned method out of the lean frame. A body that makes any call, or writes
the env by name, was refused the "skip merge" verdict that lean eligibility
required, because a callee might write a caller variable through the method's
env. Such a method ran on the general fast binder instead.

Since ADR-12529, a write reaches the caller through the variable's own cell,
so the fast binder's exit for such a body no longer merges the env. It only
carries the pending run-time-named writes out (`$::($n) = …`, an `EVAL`'d
assignment). The lean frame now does that carry too, with the same
own-name filter, and it admits a body whose merge skip is refused only for
its calls or by-name writes (#18267). A body that shares a caller container
through a parameter still takes the fast binder, and the slot frame still
admits only bodies that need no carry.
