# A method call skips two per-call lookups whose answer cannot change

Two questions were asked on every method call and answered the same way every
time.

`DispatchShape::from_instance_class` asks whether an instance's class name is a
built-in shape. A user class never is, but the answer came from a hash lookup
plus an `Attribute` prefix compare each time. A 256-slot table of the last
shapeless answers (keyed by symbol id) now makes the repeat ask one relaxed load.
The answer is a pure function of the symbol, so a racing store at worst sends a
later ask down the slow path.

`check_method_wrap_chain` cloned the invocant to hand it to
`enter_method_wrap_chain`, which then returned at once because nothing is
wrapped. The "is anything wrapped" test now runs first.

On a no-parameter method called 100,000 times, instructions went from
1,143.8M to 1,114.3M, about 295 per call (second callgrind run of each,
profiling build, `origin/main` against this branch).
