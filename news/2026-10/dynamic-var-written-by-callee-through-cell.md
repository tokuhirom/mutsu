# A routine's write to its caller's `$*x` goes straight to the variable

```raku
sub inc() { $*n++ }
sub counts() { my $*n = 0; inc(); inc(); $*n }
say counts();   # 2
```

A dynamic variable now holds its value in a shared cell from the moment it
is declared, so a routine called in its scope that assigns or increments it
writes the declaring frame's variable directly. The write used to reach that
frame only as part of copying the callee's variables back on return, which
ADR-12529 is retiring (§3, slice 39).
