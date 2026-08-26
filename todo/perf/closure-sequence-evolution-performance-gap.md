# Closure-generated evolutionary sequences are much slower than raku

The closure-generated sequence shape used by evolutionary search is functionally
correct after the sequence endpoint fixes, but it is far slower than raku. A
realistic multi-thousand-generation run therefore exceeds practical timeout
budgets even though smaller cases reach their endpoint.

## Repro

This is a small, self-contained benchmark. It does not depend on any external
program or ecosystem module:

```raku
constant target = "METHINKS IT IS LIKE A WEASEL";
constant @alphabet = flat 'A'..'Z',' ';
constant C = 10;

sub mutate(Str $string, Real $chance where 0 ≤ * < 1) {
  $string.subst: /<?{ rand < $chance }> . /, @alphabet.pick, :global
}
sub fitness(Str $string) { [+] $string.comb Zeq target.comb }

my $seed = "A" x 29;
for ^1000 {
  max :by(&fitness), mutate($seed, .001) xx C;
}
```

Measured on the debug binary:

- `raku`: approximately **0.57 seconds** for 1000 iterations.
- mutsu: approximately **48 seconds** for 1000 iterations.

The same closure-generated sequence with a distant deterministic endpoint is
correct and fast, for example `my @values = 1, { $_ + 1 } ... 300`.

## Root cause hypothesis

The cost is concentrated in the combined generator rather than in sequence
termination itself. Separate measurements for 1000 iterations on mutsu were:

- `fitness($seed)`: 0.14 seconds
- `max :by(&fitness), |@candidates`: 4.10 seconds
- `mutate($seed, .001)`: 3.25 seconds
- the combined `max` plus ten `mutate` calls: 48.37 seconds

Each generation repeatedly crosses the compiled-closure/runtime boundary for
the `:by` key extractor and for the regex assertion in `subst`. The likely fix
requires profiling and then reducing repeated closure compilation/dispatch,
regex assertion overhead, or intermediate allocation. Do not address this by
raising or removing sequence endpoint limits; that only exposes the performance
gap and does not make the program practical.

## Affected files

- `src/runtime/builtins_collection_extrema.rs` — `max :by` key extraction and
  closure calls.
- `src/runtime/sequence.rs` — closure-generated sequence pulling.
- regex substitution and inline-code execution paths used by `subst`.
- compiled closure dispatch and any repeated `eval_block_value`/slow-path calls
  reached from these operations.

This is a profiling-heavy performance ticket, not a correctness regression.
