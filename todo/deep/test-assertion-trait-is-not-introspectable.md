# `is test-assertion` is a parser flag, not something the program can see

Under the vendored upstream `Test` module (`todo/tickets/vendor-real-test-module.md`)
`t/test-assertion-line-number.t` reports the wrong line:

```
$ cat tmp/ta/inner.raku
use Test;
plan 1;
sub foo-ok() is test-assertion { flunk "foo-ok" }
foo-ok;

$ raku tmp/ta/inner.raku          |  $ MUTSU_REAL_TEST=1 mutsu tmp/ta/inner.raku
# Failed test 'foo-ok'            |  # Failed test 'foo-ok'
# at tmp/ta/inner.raku line 4     |  # at tmp/ta/inner.raku line 3
```

Line 4 is the *call site*, which is the whole point of the trait: an assertion
helper should blame its caller. mutsu's native `Test` provider gets this right
through a private line-context mechanism, so the gap is invisible until the real
module runs.

## Why the real module cannot see the trait

`Test.rakumod` does not ask mutsu for anything special. It declares the trait
itself (line 67) and then reads it back through the MOP during the backtrace
walk (line 824):

```raku
multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) is export {
    $r.^mixin( role is-test-assertion {
        method is-test-assertion(--> True) { }
    }) if $test-assertion;
}
...
    $tester = callframe($level)              # the next one should be reported
        if nqp::can(code,'is-test-assertion');
```

Three separate things have to work for that, and none of them does.

**1. The user `trait_mod:<is>` is never called.** mutsu's parser consumes
`is test-assertion` itself — `src/parser/stmt/sub/traits.rs:162` sets an
`is_test_assertion` flag on the declaration — so no trait handler runs:

```
$ mutsu -e 'multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) { note "CALLED" };
           sub foo() is test-assertion { 1 }; say "done"'
done
```

Compare `news/2026-08/...` / #5689, which fixed the *opposite* direction (a user
`trait_mod:<is>` that matches nothing must keep the builtin trait). Here a user
candidate that *does* match is skipped, because the trait never reaches dispatch
at all. Any natively-recognised sub trait presumably has the same problem.

**2. `.^mixin` does not exist.**

```
$ mutsu -e 'sub bar() {1}; role R { method zz(--> True) {} }; &bar.^mixin(R)'
No such method 'mixin' for invocant of type 'Perl6::Metamodel::ClassHOW'
```

`^mixin` is the in-place form of `but`/`does`: it re-types the *existing*
object rather than returning a new one, which is what makes it usable from a
trait handler (the Routine the declaration installed must be the one that gains
the method). mutsu has value mixins but no MOP entry point for them.

**3. Nothing answers `is-test-assertion` afterwards.** Consequently
`&foo.can('is-test-assertion')` is `False` and `nqp::can(&foo,
'is-test-assertion')` is `0`, so the walk never shifts to the caller frame.

## What is already in place

`callframe` is closer than expected — it is *not* the blocker here:

```
                         raku                       mutsu
callframe(1).code        Sub  (name inner)          Sub  (name inner)
callframe(2).code        Sub  (name middle)         Sub  (name middle)
callframe(2).line        10                         10
```

The one divergence is which line a frame reports: raku gives the line currently
executing *in that frame* (frame 1 = line 2, the `for` statement), mutsu reports
the innermost line for every frame. For this particular walk that does not
matter — the frame above `foo-ok` is the mainline and its line is already 4 —
so fixing the three items above should be enough. The frame-line divergence is
tracked separately in
`todo/tickets/callframe-line-and-file-come-from-different-frames.md`.

## Why it is large

Item 1 changes when sub traits are resolved: a natively-recognised trait name
would have to check for a matching user `trait_mod:<is>` candidate *first* and
fall back to the builtin meaning, which is the same
declaration-time-vs-runtime ordering problem as `EXPORTHOW::DECLARE`. Item 2 is
a new MOP method whose semantics are in-place re-typing of an already-installed
Routine — it interacts with `compiled_fns`, the routine registry, and whatever
identity `callframe(...).code` hands back. Neither is a one-file change, and
they are only useful together.
