# A dynamic-var read before a LATER, differently-scoped `my $*x := ...` wrongly throws X::Dynamic::Postdeclaration

Found 2026-08-20 while doing the bonus verification for the (now-fixed)
`class-level-atomicint-attribute-default-first-instance-wrong` ticket:
running `modules/Log-Timeline`'s `t/logging.rakutest` with that fix applied
progresses well past the atomicint blocker (through test 8) and then aborts
on test 9 with `X::Dynamic::Postdeclaration with no message`, in
`Log::Timeline::Model.rakumod`'s `log` method:

```raku
multi method log(&task, *%data) {
    with PROCESS::<$LOG-TIMELINE-OUTPUT> {
        my $ongoing = self!start-internal($_, $*LOG-TIMELINE-CURRENT-TASK // Nil, %data);
        LEAVE $ongoing.end();
        do {
            my $*LOG-TIMELINE-CURRENT-TASK := $ongoing;   # line 101
            &task.count == 0 ?? task() !! task($ongoing)
        }
    }
    ...
}
```

`$*LOG-TIMELINE-CURRENT-TASK` is READ earlier in the method body (as an
argument to `self!start-internal`), then a DIFFERENT, inner `do {}` block
declares its own `my $*LOG-TIMELINE-CURRENT-TASK := $ongoing`. In real Raku
these are unrelated: the inner `my $*...` is scoped to the `do {}` block and
does not retroactively apply to the earlier read (which resolves through the
normal dynamic-variable chain, or to `Any`/`Nil` if nothing set it). mutsu
incorrectly treats the later inner declaration as making the EARLIER read
"before declaration" and throws `X::Dynamic::Postdeclaration`.

## Minimal repro

```raku
class Foo {
    method go(&task) {
        say $*CUR // 'none';
        do {
            my $*CUR := 42;
            task();
        }
    }
}
Foo.new.go(-> { say $*CUR // 'none' });
```

- raku: `none` then `42`.
- mutsu: prints `none`, then throws `X::Dynamic::Postdeclaration with no
  message` at the `my $*CUR := 42;` line.

## Notes

- The offending check is a compile-time (or early-binding) heuristic --
  search `X::Dynamic::Postdeclaration` in `src/compiler/stmt.rs` (around the
  "dynamic variable used before declaration" comment) and
  `src/compiler/helpers_dynamic.rs`. It appears to conflate "read
  textually-before, in an ENCLOSING scope" with "read before a `my $*x`
  declared in the SAME scope" -- the former is legal Raku (the earlier read
  is just a normal dynamic-variable lookup that predates the inner
  binding's existence and should not see it at all), the latter is the
  actual illegal case the check is meant to catch.
- Not yet root-caused against compiler internals; this file only isolates
  and records the minimal repro. Needs a fresh investigation (AST dump +
  `rust-gdb` on the check site) before attempting a fix.
- Blocks `modules/Log-Timeline`'s `t/logging.rakutest` test 9 onward (task
  start/end logging via `.log(&task)`), now that the atomicint blocker
  ahead of it is fixed.
