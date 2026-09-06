# An array argument's mutation is lost on the SECOND call through a slurpy relay

Found 2026-09-06 while taking `Template6` 0.16.0 from 0/12 to 10/12 test files
(see `news/2026-09/template6-split-captures-and-topic-writeback.md`). This is
one of the two remaining blockers for that dist: `t/02-for.rakutest`.

A closure stored in a hash mutates its `@stack` parameter. The mutation reaches
the caller's array the *first* time the relay sub is used, and is silently lost
on every call after that.

```raku
my %handlers =
    push => -> @stack, $a { @stack.unshift('for'); 'pushed' },
    pop  => -> @stack { "popped({@stack.shift // 'EMPTY'})" };

sub act(@stack, *@stmts) {
    my $name = @stmts.shift;
    %handlers{$name}(@stack, |@stmts)
}

sub run() {
    my @s;
    my $a = act(@s, 'push', 'x');
    my $b = act(@s, 'pop');
    "$a/$b"
}

say run;   # raku: pushed/popped(for)   mutsu: pushed/popped(for)
say run;   # raku: pushed/popped(for)   mutsu: pushed/popped(EMPTY)
```

The first `run` is correct in both. On the second `run`, mutsu's `push` handler
still reports `'pushed'` but the `unshift` never reaches `@s`, so `pop` finds an
empty array. Neither the class wrapper nor the `subset`-typed array in the
original is needed — the shape above is the whole thing. It smells like a
compiled-closure / call-cache path that is taken only once the callee is warm
and that binds the array parameter by value instead of by container.

In the dist this shows up as `Template6::Parser`'s `%!directive-handlers`:
`for` does `@control-stack.unshift('for')` and the matching `end` does
`my ControlState $closed-directive = @control-stack.shift`, so the second
`Parser.compile` in a process dies with

```
Type check failed in assignment to $closed-directive; expected ControlState but got Failure
```

Dist-level repro (with `Template6` 0.16.0 unpacked and `-I lib`):

```raku
use Template6::Parser;
my $p = Template6::Parser.new;
say $p.compile("a[% for li in ul %]x[% end %]b").defined;   # True
say $p.compile("a[% for li in ul %]x[% end %]b").defined;   # dies under mutsu
```

Likely area: the compiled-closure call path (`src/vm/vm_closure_dispatch.rs`,
`call_compiled_closure_in_unit`) and its argument binding for `@`-sigil
parameters, plus whatever per-`CompiledCode` warm-up switches behaviour between
the first and later calls. It is **not** the JIT: `MUTSU_JIT=off` reproduces
identically, so the state that changes between the two `run` calls lives in the
interpreter's own compiled-function/closure caching.
