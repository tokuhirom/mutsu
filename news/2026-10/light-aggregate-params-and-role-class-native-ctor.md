# Plain `@`/`%` parameters and role-composing classes take the fast paths

Two general call costs were most of YAMLish's post-parse walk (#12917).

A routine or method with a plain `@x`/`%x` positional parameter always went
through the general signature binder, which bound the caller's container and
then wrote the parameter's final value back on exit. mutsu's arrays and hashes
are shared by reference and assignment stores in place, so that writeback
added nothing; the positional-light sub path and the fast method binder now
bind such a parameter directly when the argument is an `Array`/`List` or
`Hash`. `sub f(%h)` went from ~40.7k to ~8.1k instructions per call, a
four-parameter method like YAMLish's `concretize($schema, $ns, %anchors,
%callbacks)` from ~98k to ~34k.

A class that composes a role never used the native default constructor,
because the role sits in the class's `parents` and MRO and the eligibility
check only accepted classes there. It now accepts a plain user role, so
`Plain.new(...)` in YAMLish dropped from ~198k to ~29k instructions.

One warm `load-yaml` of the 60-row benchmark document: ~251M to ~218M
instructions (−13%).
