# Method-form `.sprintf` spreads a single positional container argument

The method form `$format.sprintf(*@args)` has a slurpy signature, so a single
positional container argument now spreads across the directives exactly like the
sub form `sprintf($format, @args)`, matching Rakudo. Previously mutsu's native
1-arg `.sprintf` fast path passed the whole array as one value:

```
"%d".sprintf([42])       # was 0,     now 42
"%s-%s".sprintf([1, 2])  # was "1 2-", now "1-2"
```

## Implementation

`dispatch_1arg.rs`'s native `"sprintf"` arm passed its single argument straight
to `format_sprintf`, which treats it as one value. It already deferred a bare
type-object argument to the slow-path `sprintf` arm (which routes through
`builtin_sprintf`, the source of truth for the sub form's flattening); that defer
now also covers a list-like argument (`arg.as_list_items().is_some()`), so the
slurpy spread and the directive/argument-count check match the sub form. A single
non-container argument keeps the native fast path.

## Not covered (pre-existing, both forms)

`sprintf("%d %d", 1..2)` does not flatten a `Range` into its elements — it is
counted as one argument in *both* the sub and method forms (mutsu reports an
argument-count mismatch where rakudo prints `1 2`). That is a separate gap in
`builtin_sprintf`'s argument flattening, orthogonal to this fix.
