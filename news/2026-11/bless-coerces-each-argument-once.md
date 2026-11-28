# `bless` coerces each named argument once, and only the last one counts

`self.bless(|%_)` seeds the attributes a named argument provides before it
evaluates the other attributes' initializers (an initializer may read them),
and then stores the arguments for real. Both passes coerced the argument the
same way, so a `%` attribute built its Hash twice and a `@` attribute its
Array twice. The seeding also matched every attribute against every argument,
and the class's MRO was looked up three times per construction.

The final store now reuses the seeded value, the argument/attribute pairs are
sorted once instead of scanned per attribute, and the MRO is read once.

Of several arguments naming the same attribute only the last is now assigned,
as in rakudo, whose `bless(*%attrinit)` binds them into a hash:
`self.bless(|%_, :v(3))` overrides a caller's `:v<bad>` without type-checking
it. mutsu stored every one in turn, so the earlier argument's type check
failed.

`benchmarks/bench-ctor.raku` (one run, `BENCH_DET=1`, callgrind on the
profiling build): 453.9M to 447.1M instructions and 283.3k to 269.0k
allocations. Part of #19399.
