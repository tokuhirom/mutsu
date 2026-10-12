# A method that creates closures gets its own frame root

Every compiled method now runs under a frame root of its own, including one
whose body creates closures. Such a method used to run directly in its
caller's env, because a root of its own had cost a whole-scope flatten on
every method call in its body. Until it matched, it read its caller's `$/`:

```raku
'ab' ~~ /a/;
class D { method rd2 { my @a = (1, 2).map({ $_ }); $/.raku } }
say D.new.rd2;   # was the caller's Match, now Nil (as in rakudo)
```

The flatten is gone too. Full method dispatch (native methods and the
interpreter fallback) used to collapse a scoped env before it ran, so that a
closure built there would see the whole lexical view. Closure capture now
stops at the frame root (`layered_capture`), and the callee chains its own
frame over the caller's, so the collapse bought nothing. Dropping it removes
the per-call `$_`/`$!` inserts that the unrooted methods needed (#12882,
ADR-12529).

Rooting these methods exposed one case that had worked only through the
leak. A punned role's body lexicals were staged in the caller's env before
its method ran, so a closure created by the method found them there. They
now travel with the role-parameter bindings that the method installs in its
own frame.

Instruction counts (release, `scripts/bench-det.sh`, before → after):
`bench-ctor` −3.2%, `bench-multi-dispatch` −2.0%, `bench-yaml-parse` −0.6%,
`bench-json-fast` +1.0%. The method-call, class, grammar, fib and string
rows are within ±0.1%.
