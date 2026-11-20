# The parser's success path no longer pays for alternatives it never reports

The frontend cost condition of ADR-15760 (#15760) — every case of
`scripts/frontend-cost.sh` within ADR-15334 Amendment 1's figures — had drifted to
113-120% of the pre-flip baseline. Callgrind against that baseline rebuilt from
source showed the regression was in the parser itself, not in the finalization
(which is about 6% of the frontend): the parse alone exceeded the whole baseline
frontend.

Five probes that run at nearly every term or statement now answer on their first
bytes:

- the slang-hook memo is neither hashed nor consulted while no slang hook is
  registered;
- `ws()` returns at once on a byte that starts no blank, comment or unspace;
- an ordered choice (`statement`, `primary`) keeps its failed alternatives'
  descriptions unmerged until the error is actually reported (`BestError`);
- the compound-assignment, set-operator and statement-keyword probes reject an
  input whose first bytes start none of their spellings;
- a keyword term (`True`, `pi`, `Empty`, ...) matches its text before the parser's
  scopes are searched for a type or enum value that shadows it.

`scripts/frontend-cost.sh measure`: zef-cli 103.9%, S32 exceptions/misc 93.1%,
S12 class/attributes 92.0%, S15 emoji-test 87.4%, grammar-cursor-attributes 95.7%
(was 120.0 / 113.7 / 114.3 / 114.8 / 115.4%).
