# The zero-argument cascade's receiver prologue answers through rows

Eighteen name-matching arms in front of the zero-argument method cascade are
gone (ADR-11276 §9.111). `Unicode.version`/`Unicode.NFG` and `List.from`/`List.to`
are method-table rows; a `Backtrace` and a `Capture` answer `Any`'s list methods
on their frames and positionals, the way a `Match` already did; a user subclass
of `Pair` or of a `Bag`/`Set`/`Mix` answers through the same payload mechanism as
an `Int` or `Str` subclass.

Behaviour now matching Rakudo: `$backtrace.List.elems` counts the frames (was 1)
and `.Seq` is a `Seq`; `\(1, 2, :a(3)).Seq` is `(1, 2)`; an empty list's `.to`
is `Nil`; `(True but False).Int` is 0 and `.not` is `True`; `.Pair` of a `Pair`
subclass is the instance; a typed parse error raised inside `EVAL` reports the
`EVAL` unit as its `filename`.
