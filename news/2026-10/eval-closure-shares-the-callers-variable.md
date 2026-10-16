# A closure made by `EVAL` shares the caller's variable

```raku
sub run(&c) { c() }
my $h;
my $handle = EVAL '-> { $h ~= "x" }';
run { $handle() };
$handle();
say $h;   # xx
```

A closure an `EVAL` creates over a lexical of the calling code used to capture a
copy of the variable's value. Its writes reached the variable only because each
routine that ran the closure copied its variables back to its caller on return.
The closure now shares the variable through a cell, the same container the
calling code reads and writes (ADR-12529 §3, slice 28).
