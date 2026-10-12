# Reading a shared parameter array while other threads push and shift no longer panics

An array handed to several `start` blocks as a routine parameter, and pushed
and shifted from all of them, panicked mutsu in every run:

```raku
sub work(@x) { for ^20000 { @x.push($_); @x.shift if @x > 10 } }
my @a;
await (^8).map: { start work(@a) };
```

`thread 'pool' panicked ... range start index 14 out of range for slice of
length 0`. The `push` and `shift` already ran under the container's
ADR-0068 structure stripe, but the reader in the condition did not: numeric
coercion (`@x > 10`, `+@x`) and truthiness (`if @x`, `?@x`, `.Bool`) located
the array's live range (`items[head..]`) with nothing held, and could see it
torn halfway through another thread's `shift`.

Those reads are leaf reads that run no user code, so they now take the same
stripe in the shared primitives themselves -- `Value::truthy`,
`coerce_to_numeric` and the infix operators' numeric coercion -- rather than
at one call site, for arrays and hashes alike. A single-threaded program pays
one relaxed atomic load, as for every other ADR-0068 funnel. Unsynchronized
mutation is still a race in Raku (rakudo crashes on the same program); mutsu
now answers instead of panicking (#11210).

The debug build's cross-check that runs a method-table call twice also no
longer asserts when a shared container changed between the two runs once a
second mutator thread exists: that difference is the program's race, not a
dispatch bug.
