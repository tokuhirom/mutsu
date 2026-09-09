# Whatever-ended Range slice assignments stop at the array length

Assignments such as `@d[1..*] = 9` now clip the Range to the array's current
length, matching Rakudo. They no longer grow a five-element array to 100,002
elements through the eager lazy-range prefix.

The assignment value is also the clipped slice, including `Any` padding:

```raku
my @d = 1..5;
say (@d[1..*] = 9).raku; # (9, Any, Any, Any)
say @d.raku;             # [1, 9, Any, Any, Any]
```

The existing unbounded-range helper is now used before assignment normalizes
the index. It expands only against a known positional array length; an
unbounded Range targeting a value without a positional length is left
unexpanded instead of being eagerly enumerated.

The regression coverage is in `t/whatever-ended-range-slice-assign.t`.

Closes [#7674](https://github.com/tokuhirom/mutsu/issues/7674).
