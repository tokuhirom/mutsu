# `X::Cannot::Empty.new(:action, :what).message` returns an empty string

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/X/Cannot/Empty.rakudoc:15`).

## Root cause

Real raku's `X::Cannot::Empty` has a `method message { "Cannot $.action from an empty
$.what" }` that formats its message from the `:action`/`:what` attributes at read
time. mutsu registers `X::Cannot::Empty` as a plain exception subclass
(`register_x("X::Cannot::Empty", "Exception", &[])` in
`src/runtime/runtime_init.rs:1787`) with no such `message` formatter — the internal
call sites that throw it (`src/runtime/sequence.rs`,
`src/runtime/methods_mut_substr_buf.rs`, `src/runtime/methods_call_dispatch.rs`) each
pass a pre-built literal message string, so they work by coincidence, but when *user
code* constructs `X::Cannot::Empty.new(:action(...), :what(...))` directly (the
documented, supported way to raise this exception from a custom class), `.message`
has nothing to fall back to and returns an empty string.

## Minimal repro

```raku
class Stack {
    my class Node { has $.value; has Node $.next }
    has Node $!next;
    method push($value) { $!next .= new(:$value, :$!next); self }
    method pop() {
        fail X::Cannot::Empty.new(:action<pop>, :what(self.^name)) unless $!next;
        my $value = $!next.value;
        $!next .= next;
        $value;
    }
}
my $stack = Stack.new.push(42);
say $stack.pop;
try $stack.pop;
say $!.message;
```

- `raku`: `42` then `Cannot pop from an empty Stack`.
- `mutsu` (`target/debug/mutsu`): `42` then an empty line (`$!.message` is `""`, exit
  code 0 — no crash, just a silently wrong/empty message).

## Affected files (starting point)

- `src/builtins/exception_message.rs` (the general per-exception-type `.message`
  formatter dispatch, judging by its handling of similar "Is: N, should be in RANGE"
  formatted messages for `X::OutOfRange` and friends) — needs an `X::Cannot::Empty`
  case that reads the instance's `action`/`what` attributes and formats "Cannot
  {action} from an empty {what}".
