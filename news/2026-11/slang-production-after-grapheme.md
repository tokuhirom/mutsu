# A slang production after a multi-code-point grapheme ends where it ends

Since a slang production's cursor sees the whole unit as its `.orig`
(#17473), the slang host reports where a production ended as the Match's
`.to`, which counts graphemes. The parser counts code points, so every
grapheme of several code points earlier in the unit, such as a ZWJ emoji like
`👩‍💻` (one grapheme, three code points), shifted where the parser resumed.
The shifted parse failed with "Confused" or "Missing required term", or ran
the wrong slang production. EEL 0.3's `t/38-prefix-index.t` and
`t/39-suffix-index.t` list `'👩‍💻'` among their test strings and failed to
compile. The end is now converted back to code points against the unit's text,
and both files match rakudo: 29/29 and 32/32 (#17486). The user-level
`Grammar.subparse(:pos)` counting is a separate problem (#17490).
