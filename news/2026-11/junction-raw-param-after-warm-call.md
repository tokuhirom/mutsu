# A Junction argument autothreads after a `.map` warmed the call

```raku
sub d($x is raw) { $x }
my @w = (1,2).map({ d($_) });
say d(any(1,2));   # was "Type check failed ... got Junction", now any(1, 2)
```

Calls of a routine compiled on the fly are answered from a name-keyed cache
once it has been called. Its light route already left a Junction argument to
the slow dispatch, which autothreads, but its by-name route bound the Junction
to the parameter directly, so an `is raw` parameter's `Any` constraint refused
it. A call with a Junction argument now skips that cache entirely and reaches
the autothreading dispatch (#18198).
