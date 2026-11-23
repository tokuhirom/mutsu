# A planned method call answers its owner's frame questions from the plan

On every call, the method fast binder asked four things that depend only on
the resolved method and the receiver's class:
- whether the owner is a role;
- whether the call installs role composition bindings;
- which package the body runs in;
- the role the body was composed from (`?ROLE`, interned on every call).

A method-call plan now answers them once, when it is installed, and replays
the answers, as it already did for the resolution and the fast-path gate
(#18267 D1a). The lean frame takes the body package from the plan as well.
The plan table is keyed on the registry write generation, so `augment`,
`^add_method` or a role composition still clears the answers. A plan whose
owner may be a mixin published by another thread keeps asking per call.
