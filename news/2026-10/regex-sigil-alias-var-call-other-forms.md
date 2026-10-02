# Every sigil alias on a `<$var>` call keeps the called regex's captures

A sigil alias on a subrule call names the called rule's own Match, with its
nested captures. #10522 did this for the scalar `$<a>=<$re>`. Three
neighbouring forms still kept only the matched span:

- the array alias `@<a>=<$re>`;
- the numbered alias `$0=<$re>`;
- a Str-valued variable, as in `$<a>=<$s>` and `<a=$s>`.

A Regex-valued variable under any sigil alias now becomes the non-capturing
call `<&$re>`. The alias stays on the token, so it keeps its own meaning (a
forced List, a positional slot), and both regex engines file it the same way.
A Str-valued variable under a scalar alias becomes `<a=$s>`, and that form's
match-time lookup now compiles the string as a pattern, as `<a={ code }>`
already did.

A numbered alias on any subrule call (`$0=<rule>`, `$0=<&$re>`) now puts the
called rule's Match in the positional slot, so `$0<digit>` is reachable. The
node lookup that both alias kinds share moved to
`src/runtime/regex/regex_alias_subcap.rs` (#10673).
