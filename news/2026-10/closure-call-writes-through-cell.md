# A block called on the spot writes its outer variable through the variable itself

```raku
my $a = 0;
-> $i { $a += $i } for 1..3;
say $a;   # 6
```

A closure that assigns a variable it closed over now shares that
variable's container whether it escapes the scope that made it or is
called right away (a statement-modifier `for`'s pointy block, a block
handed to a routine, a CONTROL handler's block). Before, only an escaping
closure got the shared container, and the write of one called on the spot
reached the variable by being copied back when the closure returned, a
copy-back ADR-12529 is retiring (§3, slice 42).

On the way, a push to a dynamic array (`@*LOG.push(...)`) after any thread
had run no longer works on a detached copy of the array when no thread
shares it.
