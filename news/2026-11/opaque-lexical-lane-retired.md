# Lexicals written by a class or role method no longer ride the thread lane

ADR-16595 slice 5 (#16600). After slice 4 the only user lexicals a thread spawn
still published into the name-keyed cross-thread store were the "opaque" ones:
a lexical some registered class or role method writes. Because the store is
keyed by bare name, a worker's own `my @log` then answered, and overwrote, the
caller's `@log` whenever any method in the program wrote a lexical called
`@log`:

```raku
my @log = 1, 2, 3;
class Foo { method bump { @log.push(4) } }
sub worker() { my @log = <w>; @log.push('x'); @log.join(',') }
say await start { worker() };   # w,x
say @log.join(',');             # was "w,x"; now 1,2,3 as on rakudo
```

Every such lexical already had a place a worker reaches (the declaring frame's
cell, the compunit's lexical cell, or the package's static store), so the
spawn now publishes nothing, and the opaque predicate and the
`type_body_written_lexicals` bookkeeping are gone. Removing the lane exposed
two gaps it had been hiding, both fixed here:

- Composing a role left the role body's `my` lexicals bound in the composing
  scope; a later class's `my $n` static was then read from that stale binding
  instead of its own cell, and a method run on a worker lost its write.
- A `start` block at program scope that names a lexical only through `EVAL` or
  `::('$x')` wrote the worker's copy of the program scope:
  `my $x = 1; await start { EVAL '$x = 42' }; say $x` printed `1`, now `42`.
