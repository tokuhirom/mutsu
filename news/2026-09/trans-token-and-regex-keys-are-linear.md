# `.trans` with token or Regex keys is linear again

`Str.trans` was quadratic in the subject length within a single call whenever a
rule was not a single-char map: any multi-char key or replacement (including the
common `"\n" => "\r\n"`, which LWP::Simple uses), a key whose NFC form is several
codepoints, or a Regex key (#9142).

The scan in `apply_trans_rules` / `apply_trans_complement` tries every rule at every
position. At each position the token rules collected the whole rest of the subject
into a fresh `String` (`chars[i..].iter().collect()`) just to run a `starts_with`,
and the Regex rules called `regex_match_with_captures_at`, which builds a new
`MatchTarget` — a copy of the whole subject plus its char array — for every
attempt. The complement path did both, and additionally searched the copied
remainder for the first match rather than attempting one anchored match.

Now the token keys are split into char slices once per call and compared in place
(`chars[i..].starts_with(&key)`), and one `MatchTarget` is built per call and shared
by every anchored attempt through the new
`regex_match_with_captures_at_target` (the same shape #8247 gave `split`). The
complement path uses the same anchored attempt as the main path, so a `^`-anchored
key only matches at the real start of the subject there too.

`scripts/str-complexity-check.sh trans` (release, 4-core container, one call on 4·N
chars):

| case | N | before t(2N) | after t(2N) | after ratio |
|---|---:|---:|---:|---:|
| `.trans(["ab"] => ["x"])` | 10000 | 3.88 s | 0.0017 s | 1.88 |
| `.trans("\n" => "\r\n")` | 10000 | 4.97 s | 0.0019 s | 0.98 |
| `.trans(/b/ => "x")` | 5000 | 4.29 s | 0.016 s | 2.69 |

On 320k chars `/b/ => "x"` takes 0.10 s (rakudo on the same input: 0.085 s).

While there, `methods_trans.rs` (over 900 lines) was split: the rule-building
helpers moved to `methods_trans/rules.rs` and the scanning loops to
`methods_trans/apply.rs`, and `regex_match_with_captures_at` moved out of the
over-long `regex_match_find.rs` into `regex/regex_match_at.rs`.

Pinned by `t/types/trans-long-subject.t`.
