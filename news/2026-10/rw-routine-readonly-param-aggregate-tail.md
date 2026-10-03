# Assigning through a readonly `$` parameter tail dies, aggregate or not

An `is rw` routine whose tail is a plain (readonly) `$` parameter returns a
value, not a container, so assigning to the call must die. mutsu already
refused when that value was an Int, but a Hash or Array was stored into, so a
routine could write the caller's aggregate through a parameter it was never
allowed to write:

```raku
sub w($p) is rw { $p }
my %r; w(%r) = 1;   # was: %r became {1 => (Any)}
                    # now: Cannot assign to a readonly variable or a value
```

`assign_through_rw_result` now refuses an itemized aggregate that reaches it
without a container. Every writable `$` hands back its Scalar cell before that
point, while an `@`/`%`, sigilless or `is raw` tail that aliases a real
aggregate is never itemized, so those tails still store into the caller's
container (#11108).
