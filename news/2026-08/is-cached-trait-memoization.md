# `is cached` routines now memoize results

`is cached` routines now cache successful results by argument values, matching
Rakudo's behavior. Repeated calls with the same arguments return the memoized
value without re-running the routine body, while different arguments receive
separate cache entries.

For example:

```raku
use experimental :cached;

my $calls = 0;
sub double(Int:D $x) is cached {
    $calls++;
    $x * 2;
}

double(21); # runs the body
double(21); # uses the cached result
double(22); # runs the body for a new argument
say $calls;  # 2
```

The cache is owned by the compiled routine declaration. Cached routines use
the common compiled-call boundary so all call paths observe the same cache.
The regression pin is `t/cached-trait.t`.
