# A role's 0-arg `multi method` loses a trailing literal from an interpolated string, only when the composing class has its own extra attribute

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1132, the `Notable`/`Journey` example).

## Repro (isolated by binary search from the doc example)

```
constant t = " " xx 4;
role Notable {
    has Str $.notes is rw;
    multi method notes() { "$!notes\n" };
    multi method notes( Str $note ) { $!notes ~= "$note\n" ~ t };
}
class J does Notable { has $.zzz; }
my $j = J.new;
$j.notes("First steps");
$j.notes("Almost there");
say "[" ~ $j.notes ~ "]";
```

- raku: `[First steps\n    Almost there\n    \n]`
- mutsu: `[First steps\n    Almost there\n    ]` — the final `notes()` call's own trailing `"\n"`
  is dropped

## Isolation notes

- Removing the class's extra attribute (`has $.zzz`) makes the bug disappear.
- Replacing the role's `multi method notes()` (the 0-arg variant) with a plain non-`multi`
  method also makes the bug disappear, even with the extra attribute still present.

So the bug requires **both** "0-arg `multi method` declared inside a role" and "the composing
class declares its own attribute". Since the *content* returned is correct up to the very last
character (only the method's own trailing string literal is missing), this looks like a
compiled-string-template / constant-pool interaction tied to per-class multi-method
compilation, not attribute-value corruption.

## Affected files (starting point)

- `src/compiler/` — multi-method compilation when composed into a class that has its own
  attribute list (attribute-count-dependent constant/slot allocation is the suspicious
  variable)
- `src/runtime/class.rs` — role composition, multi-method registration

## Suggested next step

Compare `--dump-ast`/bytecode for the 0-arg `notes()` method compiled standalone vs. compiled
after the class gains an attribute, to see whether a string-literal constant or template slot is
being mis-indexed or truncated when the class's attribute count changes.
