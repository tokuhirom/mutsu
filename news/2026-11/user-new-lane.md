# A class's own `new(*%_)` replays straight into its frame

`Dist.new(name => ..., ...)` on a class that declares
`method new(*%_) { self.bless(|%_) }` walked, before the method's frame was
pushed, the `CallMethodMut` argument stretch, the probe chain, the native
default constructor (which then declined, since the class has a `new` that
takes the call), the dispatch tail's accessor and wrap probes, the method
resolution and the fast-path gate. The plain-method lane and its method-call
plan skip that walk for an instance receiver; a type object had nothing.

The new user-`new` lane records, per class, the dispatch tail's outcome where
the chain reaches the class's own `new`, for a `new` that is the class's one
visible candidate and takes the invocant plus a named slurpy: then the
default constructor's decline and the fast-path gate's verdict are functions
of the class, and the three per-call readings the gate makes for such a
signature (a `Mu`-only value, an internal key, an explicit `*%h`'s argument
sources) are asked on every replay. The replay runs at the top of the opcode
and goes through the same method-dispatch frame and fast binder as the tail.
The registry generation and the class's constructor plan pin the memo.

On `bench-ctor` (callgrind, per construction): see the PR (#19399).
