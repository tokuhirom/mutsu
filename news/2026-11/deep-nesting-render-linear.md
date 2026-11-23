# `.gist` and `.raku` of deeply nested arrays render in linear time

Every renderer level used to build its own string and then copy it into its
parent's (`"[" ~ inner ~ "]"`). An array nested d levels deep therefore copied
O(d²) bytes. At depth 200,000 that was 2.6 s for `.gist` on a release build,
and `.raku` overflowed the native stack (#12714). #14821 had already removed
the quadratic ancestor scans.

The list arms of the three renderers that recurse into nested lists now append
into one shared buffer:
- `gist_value`;
- the native collection gist behind `.gist`;
- `raku_value`.

A cycle back-reference preamble (`(\Array_… = …)`, `((my @Array_…) = …)`) is
inserted at the node's start position once its walk finds the loop. The text
is unchanged.

On a release build, depths 50,000, 100,000 and 200,000 now take 0.02, 0.06 and
0.13 s for `.gist`, and 0.09, 0.15 and 0.27 s for `.raku`.
