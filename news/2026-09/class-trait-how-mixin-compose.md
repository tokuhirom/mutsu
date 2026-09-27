# A class trait's `$class.HOW does Role` now runs the role's `compose`

A `trait_mod:<is>` on a class that mixes a role into the class's own
meta-object (`$class.HOW does SomeHOWRole`) is how modules such as
Staticish hook class composition. mutsu mixed the role in but never called
its `compose` override, so the hook silently did nothing. The class
declaration now queues that `compose` on the same drain the EXPORTHOW
metaclass hook uses, after the class's traits have run — once, at the real
declaration and not for the hoisted forward-reference shell. A re-registration
also drops the previous pass's mixed HOW, together with the wrap chains it
already cleared, so the hook re-applies what it installed.

Getting Staticish's singleton wrapper to run end to end exposed six more
gaps, all general:

- `callsame` inside a role method mixed into a native HOW now reaches the
  native metamethod (`compose`, `add_method`, ...), the same way it already
  did from a user `Metamodel::ClassHOW` subclass.
- That native `compose` step is now where a class's auto-generated accessors
  appear in `.^method_table`: hidden before the hook's `callsame`, visible
  after it, as in Rakudo. Before, they stayed hidden for the whole hook.
- `.^find_method` and `.^lookup` on a `does`/`but` mixin find the mixed-in
  role's methods (before, they returned `Mu`).
- A `Method` object used as a plain callable, such as a `.wrap` wrapper taken
  from `.^find_method`, runs its method. Before, this died with "Callable
  expected".
- A role body's own lexicals (`my %bypass = ...`) persist for a runtime mixin
  the way they already did for a pun. Before, they were lost once the frame
  that did the first `does` returned.
- A `.wrap`ped auto-accessor called on the type object runs its wrapper
  instead of dying right away. The wrapper can then supply an instance.

Staticish's `t/020-test.t` still needs two larger features, both filed:
wrapping a multi method's dispatcher, and an `is rw` accessor handing its
attribute container back through a wrapper.
