# A closure's captured variable passed to an `is raw` method parameter binds on the fast path

A method parameter that aliases the caller's container (`Mu $x is raw`, `\x`) binds on
the method fast binder only when every such argument's container can be classified there.
An argument the call site wrote as a variable that is not a slot of the running frame
used to send the call to the general binder. Such a variable is one a closure captured
from the routine around it, like AttrX::Mooish's FETCH closure passing the enclosing
method's `$instance` to `build-attr`. The general binder looks the name up and aliases
the shared cell it finds there. The fast binder now relays that same cell when the env
holds one under the name and the argument is its current contents.

A call with a `Capture` argument (`store-with-cb(..., \( :builder ))`) is now admitted
to the method-call plan as well: the lane already keyed a Capture by its class, and the
fast binder type-checks it on every call. The container-argument promotion no longer
collects the positional indices into a vector on every call.

On AttrX::Mooish's `t/090-concurrent.custom` probe, `build-attr`, `invoke-opt` and
`attr-var` now replay their call plan. A round went from 5.310M to 5.077M instructions
(-4.4%), and the general binder's share of a round fell by about two thirds (part of
#18267).
