# The plain-method lane now admits method calls with arguments

The `CallMethodMut` plain-method lane (#8880) lets a user-class method call
skip the pre-dispatch probe chain. Those probes check whether the receiver
might be an exception, a proto, an accessor, an `IO::Handle` and so on. The
lane applies once a dispatch for the same receiver class and method name has
already walked that chain without any probe claiming it. Until now the lane
admitted only calls **without** arguments, so `$shape.scale($x)` paid the whole
chain on every call.

The lane key now also carries each argument's type key: its runtime type plus
its definedness, the same key the multi-dispatch caches use. A call whose
arguments cannot be keyed never enters the lane: a `Junction`, a named `Pair`,
a container or a mixin. Those are the shapes that the argument-reading probes
(autothreading, the named-argument intercepts) decide on. The one probe whose
decline depends on argument values, the native-method cascade, runs only when
the class has no user method of that name. So a call with arguments installs
the lane only when the class does have one.

Callgrind, instructions per call, against main:

| shape | main | now |
|---|---|---|
| `$o.m(3)` on a plain method | 24.5k | 21.3k |
| the `bench-multi-dispatch` multi method loop body | 43.3k | 40.0k (-7.6%) |
| `$o.m()` (already on the lane) | 17.7k | 17.7k |

Part of #10111.
