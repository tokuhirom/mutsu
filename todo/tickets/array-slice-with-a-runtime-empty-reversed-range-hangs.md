# `@a[0 .. $n]` hangs forever when `$n` holds a negative value

An array slice whose range endpoint comes from a **variable** that happens to
be negative loops forever instead of producing the empty list.

## Repro

```
my @el = (4,); my $e = -1; say @el[0 .. $e].raku;
```

- mutsu: hangs (killed at `timeout 10`, exit 124)
- raku:  `()`

## Controls that are CORRECT

```
my $e = -1; say (0 .. $e).elems;   # 0 both     <- the Range itself is fine
my $e = -1; say (0 .. $e).raku;    # 0..-1 both
my @el = (4,); say @el[0..0].raku; # (4,) both
my @el = (4,); say @el[0 .. -1];   # errors in BOTH (a literal negative endpoint is rejected)
```

So the Range is constructed correctly and is correctly empty; it is the
**subscript** path that fails to notice, and it only fails when the endpoint
arrives through a variable — the literal spelling is rejected earlier by both
implementations, which is why this was never noticed.

## Why it matters

A hang is worse than a wrong answer: it takes out the whole process with no
diagnostic, and `0 .. $n-1` over a computed length is one of the most common
loop/slice idioms there is. This is the actual cause of the
`Language/objects.rakudoc:1397` doc-diff timeout, which had been attributed to
the BinaryTree role example.

## Where to look

The positional-slice arm that expands a `Range` subscript into indices. It
presumably iterates from `.min` while comparing against `.max` in a way that
never terminates when `max < min`, instead of checking `elems == 0` (or
`is-empty`) first. The `Range` value itself already reports `0` elements, so
the information is available at the call site.

## Acceptance

- The repro answers `()` and terminates.
- Every control above still behaves as recorded, including the literal
  `@el[0 .. -1]` continuing to error in mutsu the way raku does.
- A `t/` pin with a `timeout`-bounded run, covering `@a[0..$neg]`,
  `@a[$neg..$neg]`, and the associative twin if it shares the path.
