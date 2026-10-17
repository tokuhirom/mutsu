# A `my &*name` no longer outlives the frame that declares it

`my &*cb = { ... }` inside a routine or block stayed visible after the routine
returned or the block exited (#13705):

```raku
sub h() { my &*cb = { 1 }; 2 }
h();
say (try &*cb) // 'no cb';   # rakudo: no cb; mutsu printed the block
```

A `my $*x` marks itself as the frame's own binding of the dynamic, so a store
to it is never taken for a write to the process-level `PROCESS::<$x>`. The
`&*` form did not get that mark. Its first store also landed in a frame slot
the env does not mirror, so the store found no env binding and was published
as the process binding, which every later frame read. The declaration now
marks `&*` names too, and a dynamic declared in scope without an env binding
yet is never taken for the process one.

Reading a `&*name` that no frame in the dynamic scope declares is now the
lazy `X::Dynamic::NotFound` Failure, as a `$*name` read is. Calling one
(`&*nope()`) throws `X::Dynamic::NotFound`; before, the read gave `Nil` and
the call died with "Unknown function".
