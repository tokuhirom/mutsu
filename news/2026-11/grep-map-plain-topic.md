# A grep/map block that cannot see its topic's container binds the plain value

`.grep({ $_ %% 3 })` and `.map({ $_ * 2 })` bound each element's container cell
as the callback's `$_`. That cost a clone and a drop of the cell per binding,
and each drop ran the cycle collector's acyclicity check. On top of that came a
grep write-back key, a readonly scope and the topic's readonly marking, for
every element (#19323).

The inline loops now ask the analysis that `for` loops already use,
`topic_alias_unobservable`. When the body cannot write, bind or capture `$_`,
cannot call user code and cannot reach the source by another name, the loop:

- binds the element's plain value as the topic;
- skips the write-back key;
- opens no readonly scope when the block binds no parameter.

`%%` and `!%%` are added to the analysis' pure operators.

`benchmarks/array-ops.raku` (callgrind, JIT on, second run) goes from
730,123,170 to 539,986,655 instructions, **-26%**. Per callback invocation
that is about 5,500 → 4,050 instructions. The allocation count is unchanged:
the loop already made no per-element allocation.

A follow-up compiles such a body with a hidden local slot for `$_`. Each
element is seeded into that slot. The env gets no `_` / `$_` binding at all
when the code names the topic nowhere by name and makes no closure. The
body's value now comes back on the stack instead of through the env's `_`.
`benchmarks/array-ops.raku` goes from 539,986,655 to 398,235,306
instructions, another -26% (-45% in total). That is about 2,990 instructions
per callback invocation; rakupp v5.9.0 spends about 3,200.

As a side effect, `(1, 2).map({ say $_ })` now yields `(True True)`, as in
rakudo. A body that can observe its topic still yields the element there
(#19355).
