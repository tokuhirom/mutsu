# A sub keeps its mainline variable when several blocks shadow it

```raku
my $q = 'top';
sub f { $q }
if True { my $q = 'a'; say f() }   # top
if True { my $q = 'b'; say f() }   # top (was: b)
```

A named sub that reads a mainline lexical resolves it through the cell it
captured when it was declared (ADR-0024). The first block that shadowed the
name restored the outer value on exit into every slot of that name, including
the next sibling block's. That block's `my $q` then looked like a
re-declaration of the captured variable, and the capture was dropped. From the
second shadowing `if` branch, loop body or bare block on, the sub read its
caller's `$q`, and its writes went there too. The re-declaration check now also
requires the slot to be the one the sub captured (ADR-12529 phase 3, slice 17).
