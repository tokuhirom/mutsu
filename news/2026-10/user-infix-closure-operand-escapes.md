# A closure operand of a user infix operator is compiled as an escaping closure

A user-defined infix operator receives a closure literal operand the same way
a routine receives a closure argument, and it may keep it:

```raku
my @later;
sub infix:<later>(&c, $x) { @later.push(&c); $x }
my $n = 0;
{ $n++ } later 1;
.() for @later;
say $n;   # 1
```

The operand was compiled as a non-escaping closure, so a lexical it writes was
not shared through a cell with the frame that created it, and only the call's
return merge carried the write back. It is now compiled exactly like a call
argument (ADR-12529 §3, slice 26). The slice also reran the census of what
still depends on the return merge: 12 files remain, down from 26.
