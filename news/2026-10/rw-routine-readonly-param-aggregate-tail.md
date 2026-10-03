# Assigning through a readonly `$` parameter tail dies, aggregate or not

An `is rw` routine whose tail is a plain (readonly) `$` parameter returns a
value, not a container, so assigning to the call must die. mutsu already
refused when that value was an Int. When it was a Hash or Array, mutsu stored
into it, so a routine could write the caller's aggregate through a parameter it
was never allowed to write:

```raku
sub w($p) is rw { $p }
my %r; w(%r) = 1;   # was: %r became {1 => (Any)}
                    # now: Cannot assign to a readonly variable or a value
```

A bare Hash or Array result cannot tell the two cases apart:
`@n[1]:v = 31` legitimately list-assigns into an itemized element. So the
compiler now records each routine's readonly `$` parameters, meaning no
`is rw`, `is raw` or `is copy` trait and not sigilless. A `return-rw` or
`is rw` tail naming one of them emits the new `MarkReadonlyRwTail` opcode. The
routine-call assignment then refuses a result that is that very value. Tails
that alias a real aggregate (`%p`, `\p`, `$p is raw`) still store into the
caller's container (#11108).
