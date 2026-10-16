# Runtime-named writes cross every kind of call return explicitly

```raku
our $g = 1;
sub set-g($n) { $::($n) = 5 }
sub via($n) { set-g($n) }
via('g');
say $g;   # 5
```

A write whose target is named only at run time -- `$::($n) = v`, or a
`CALLER::CALLER::.BIND-KEY` -- is now carried back to the frame that owns the
variable by every call path. The lighter call paths used to get it there only
as a side effect of copying the callee's lexicals back on return, which
ADR-12529 is retiring (§3, slice 36).
