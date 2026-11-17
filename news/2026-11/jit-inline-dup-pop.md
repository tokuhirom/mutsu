# The JIT copies and drops plain words on the stack in native code

Every `&&`, `||`, `?? !!` and `//` chain compiles to `Dup`, a conditional jump
and `Pop`. The JIT's Tier B already emitted the comparisons and the Bool
jumps of such a chain inline, but `Dup` and `Pop` still left the native code
for a full interpreter dispatch (`vm_jit_helpers::step`, ~170 instructions)
each time. A character-class predicate such as MCP::Server::Tool::Web's

```raku
my sub is-name-char(Str $ch --> Bool:D) {
    return False if $ch eq '';
    my $ord = $ch.ord;
    (0x41 <= $ord <= 0x5A) || (0x61 <= $ord <= 0x7A) || (0x30 <= $ord <= 0x39)
        || $ch eq '-' || $ch eq '_' || $ch eq ':' || $ch eq '.';
}
```

made 18 such round trips per call, about a third of the ~10.7k instructions
the call cost.

`Dup` and `Pop` are now emitted inline when the top word owns nothing — a
small Int, a Num, a `Bool` or a type object — which is a word copy or a length
decrement. Any other word (a string, `Nil`, a big Int, an array, a lazy list
that a sinking `Pop` must drain) and an empty or full stack still take the
interpreter arm. On the tokenizer's `read-name` loop this is 12.5% fewer
instructions (callgrind, 4.63G → 4.06G for 216k characters).
