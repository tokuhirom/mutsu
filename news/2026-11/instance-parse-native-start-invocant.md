# A grammar parse called on an instance builds its start invocant natively

A parse runs its start rule on `self.new` (#10848). For a type-object
receiver (`G.parse`) that object came from the native default constructor,
but for an instance receiver (`G.new.parse`, which is how YAMLish's
`concretize` parses every plain scalar with `$schema.new.parse($!value)`) it
went through the generic method-call path: about 54k instructions per parse,
spent resolving `new` by name.

With no user `new` in the MRO and no replaced BUILDPLAN, `self.new` on an
instance is `Mu.new`, which builds a fresh object of the invocant's class. That
is the object the native constructor builds, so an instance receiver now uses
it too. A user `new` still gets the general call, on the instance.

`Schema::Core.new.parse("value 1 padded   here")` (`profiling` build,
`MUTSU_JIT=off`, callgrind): **332k -> 274k instructions per parse (-17%)**,
55 -> 43 us locally (rakudo 33 us). One warm `load-yaml` of
`bench-yaml-parse-big.raku`, which does 60 such parses: 141.8M -> 138.2M
instructions (-2.5%). Refs #12917.
