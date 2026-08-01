# `$?FILE` inside a module names the main script, and `callframe` skips the module's frames

Two related gaps in mutsu's compile-time file constant and its runtime frame
stack. Together they defeat any routine that answers "where was I called from,
outside my own file" — the standard shape for a test-assertion library's failure
report.

## Repro

`tmp/core/CF2.rakumod`:

```raku
unit module CF2;

sub inner-report() {
    my int $level = 1;
    say "MODULE \$?FILE = {$?FILE}";
    loop {
        my $f = callframe(++$level);
        say "level $level: ", ($f.defined ?? "{$f.file} line {$f.line}" !! "(undefined)");
        last if $level > 5 || !$f.defined;
    }
}

sub report() is export { inner-report }
```

driven by `use CF2; report;`:

```
--- mutsu ---
MODULE $?FILE = tmp/core/cf2.raku
level 2: tmp/core/cf2.raku line 2
level 3: tmp/core/cf2.raku line 1
level 4: (undefined)

--- raku ---
MODULE $?FILE = /home/…/tmp/core/CF2.rakumod (CF2)
level 2: /home/…/tmp/core/CF2.rakumod (CF2) line 13
level 3: tmp/core/cf2.raku line 2
level 4: tmp/core/cf2.raku line 1
level 5: NQP::src/HLL/Compiler.nqp line 197
level 6: NQP::src/HLL/Compiler.nqp line 423
```

1. **`$?FILE` is the main script inside a module.** It must be the file the
   enclosing compilation unit was compiled from.
2. **`callframe($n)` skips the module's own frames.** `report` (a frame in
   `CF2.rakumod`) does not appear at all; level 2 is already the caller script.
   Frame *lines* are also off by one against raku in a plain single-file chain
   (`sub middle { inner }` reports the `inner` call site, not the `middle` one).
3. `callframe` past the outermost frame returns an undefined value; raku keeps
   going into the compiler's own frames. That difference is defensible on its
   own, but callers written against raku (see below) walk until `.file` stops
   matching, so an undefined value where raku still has a frame turns into a
   `No such method 'file' for invocant of type 'Any'`.

## Why it matters

rakudo's `Test.rakumod` reports a failing test's location with exactly this
walk (`sub proclaim`):

```raku
repeat {
    $caller = callframe(++$level);
} while $?FILE.ends-with($caller.file)
     || $caller.file.ends-with($?FILE);
```

With `$?FILE` equal to the *script* rather than to `Test.rakumod`, the very
first comparison matches the script frame, so the loop walks off the end of the
stack and dies on `Any.file`. Running the genuine upstream module therefore
works for every passing assertion and dies on the first failing one — see
`todo/tickets/vendor-real-test-module.md`.

```
$ mutsu -I tmp/core -e 'use Test2; plan 1; ok 0, "deliberate failure"'
1..1
not ok 1 - deliberate failure
No such method 'file' for invocant of type 'Any'
  in sub proclaim at tmp/core/Test2.rakumod line 813
```

raku prints `# Failed test 'deliberate failure'` / `# at … line 3` instead.

## Notes

`$?FILE` is already correct for `--dump-ast`-visible mainline code and for the
backtrace frames mutsu prints on an exception (`t/backtrace-module-file.t` pins
that a sub defined in a used module reports the *module* file), so the module
path is available at the point the constant is folded — the two mechanisms just
do not share it. Start there rather than by changing `callframe`.
