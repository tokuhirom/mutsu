# A private method call replays its method-call plan

`self!name(...)` used to rebuild the whole private dispatch on every call. It
resolved the private method, ran the full fast-path gate over the signature
(`call_compiled_method`) and then the binder. A public method call skips that
from its second call on, by replaying its method-call plan (ADR-15737 D1).

The private dispatch arm now records a plan for a lane hit, as the public
dispatch tail does. The next call with the same receiver class and argument
types goes straight to the fast binder, or to the lean or slot frame when the
method qualifies. The calling context's permission is still checked on every
call (`can_fast_dispatch_private_method_vm`): a caller the arm would refuse
takes the whole path, as it did before. A plan is dropped on any registry
write, so a run-time `^add_method`, a wrap or a role composition invalidates it
exactly as it does a public method's plan. Private `multi` methods are not
planned.

Found along the way: a parent's `self!m` resolves to a subclass's private `!m`
of the same name (#18831), which the plan neither causes nor changes.
