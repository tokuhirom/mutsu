# The RakuAST frontend notes each term once

Under `MUTSU_RAKUAST=1` a unit is parsed with its spellings and term origins kept
(ADR-10723 Stage 2, `parser::term_origins`), and the parser noted every term it read
together with a structural fingerprint. A term passes through several parsers
(`primary`, the postfix loop, the prefix level) and through the `primary` memo on every
backtrack, and each of them noted what it returned: 85,844 notes for 6,836 distinct
(fingerprint, range) pairs on the first 1,500 lines of `roast/S15-nfg/emoji-test.t`.

Now a remembered `primary` term is not noted again, a postfix loop that consumed nothing
does not note the term it was handed, and a note that repeats the previous one's
positions and node kind is skipped, which leaves 25,948 notes and the same 6,836
distinct pairs. The fingerprint uses `FxHasher`, the notes are deduplicated with an
unstable sort, and the round trip works on the unit it was handed instead of a copy.

Frontend cost (`mutsu -c`, callgrind instructions, first 1,500 lines of
`emoji-test.t`): legacy 816.6M; `MUTSU_RAKUAST=1` 1,238.0M before, 995.9M after
(+52% to +22%). ADR-10723 §2.4 asks for +10%, so #15011 stays open.
