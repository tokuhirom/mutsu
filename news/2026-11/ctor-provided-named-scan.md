# A constructor asks its named arguments by name instead of hashing them

Before `BUILD`, the default constructor checks every `:D`/`:U` attribute, and
a required `:D` attribute that was *passed* an undefined value fails the
assignment type check, while one that was not passed at all is the required
error. To tell the two apart it collected the call's named-argument keys into a
`HashSet<String>` on every construction: a SipHash and a `String` copy per key,
then another hashed lookup per smiley attribute in the set of required names.

The keys are now asked by a scan over the call's own Pairs
(`ProvidedNamed`), which is cheaper than building any set for the handful of
arguments a constructor gets, and each smiley rule carries its is-required
flag, settled once per class shape. `dispatch_new`'s required check uses the
same scan.

On #19384's `Quoted.new` reproducer (roles with typed `is required`
attributes) this is 39.4k to 34.7k instructions per call (callgrind, profiling
build, on a main without #19393/#19395). Part of #19399.
