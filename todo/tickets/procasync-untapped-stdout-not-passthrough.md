# `Proc::Async` output is silently dropped when nobody `.tap`s it, instead of passing through

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Type/Proc/Async.rakudoc:102`, `:110`, `:233`).

## Root cause hypothesis

When a `Proc::Async`'s `.stdout` (or `.stderr`) `Supply` is never `.tap`ped, real Rakudo
lets the child process's stdout/stderr **inherit the parent's real stdout/stderr** (or, for
`bind-stdin`-chained processes, feed straight into the bound consumer) — nothing captures
it into an internal buffer that then goes nowhere. mutsu's `.start()`
(`src/runtime/native_proc_async.rs`) unconditionally does
`cmd.stdout(Stdio::piped()).stderr(Stdio::piped())` regardless of whether a tap exists, so
when nothing ever reads that pipe, the child's output is captured into a channel that is
simply never drained — the output is lost rather than shown.

## Minimal repro

```raku
my $prog = Proc::Async.new(:w, 'hexdump', '-C');
my $promise = $prog.start;
await $prog.write(Buf.new(12, 42));
$prog.close-stdin;
await $promise;
```

- `raku`: prints the hexdump output directly to the terminal (child's stdout passes
  through since nobody tapped it):
  ```
  00000000  0c 2a                                             |.*|
  00000002
  ```
- `mutsu`: prints nothing.

Same root cause via piping one `Proc::Async`'s stdout into another's stdin
(`bind-stdin`) when neither process's output is tapped:

```raku
my $proc-echo = Proc::Async.new: 'echo', 'Hello, world';
my $proc-cat = Proc::Async.new: 'cat', '-n';
$proc-cat.bind-stdin: $proc-echo.stdout;
await $proc-echo.start, $proc-cat.start;
```

- `raku`: `     1\tHello, world` (from `cat -n`, printed to the parent's real stdout since
  nobody tapped `$proc-cat.stdout`)
- `mutsu`: prints nothing.

And via `bind-stdin` from a real file, again with nothing tapped:

```raku
my $p = Proc::Async.new("cat", :in);
my $h = "/etc/profile".IO.open;
$p.bind-stdin($h);
$p.start;
```

- `raku`: prints the file's contents (via `cat`'s pass-through stdout).
- `mutsu`: prints nothing.

## Affected files (starting point)

- `src/runtime/native_proc_async.rs` — `.start()`'s `Command::new(&program)` setup
  (`cmd.stdout(Stdio::piped())`/`cmd.stderr(Stdio::piped())`) should only pipe when there is
  an actual tap (or a `bind-stdin` consumer, or binary/text mode was explicitly requested)
  registered on the corresponding `Supply`; otherwise it should use `Stdio::inherit()` so
  the child's output reaches the parent's real stdout/stderr, matching Rakudo.
