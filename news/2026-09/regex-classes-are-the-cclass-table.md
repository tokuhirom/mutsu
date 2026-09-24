# Regex `\d` / `\w` / `\s` / `\n` / `<alpha>` are MoarVM's character classes

`nqp::iscclass` answered from MoarVM's `CCLASS_*` table. The regex engine answered the same
questions with Rust's `char` predicates, and the two disagreed on about 1,400 codepoints below
U+3000:

- `\d` and `<digit>` were ASCII only. `"٣" ~~ /\d/` and a fullwidth `５` failed to match, where
  rakudo's `\d` is every script's `Nd`.
- `\w`, `<alpha>` and `<alnum>` used `char::is_alphanumeric`. That admits `²` (No), `Ⅰ` (Nl) and
  combining marks such as U+0345, and rakudo's classes reject all of them.
- `\n` did not match VT, FF or U+2029, and neither did `<[\n]>`. `\N` inside a class treated
  only LF and CR as newlines.
- `<blank>` missed every `Zs` space except U+0020 and U+00A0.

The table now lives in `builtins::cclass`, which `nqp::iscclass`/`findcclass` and the regex engine
share (ADR-0118 §2.5). The engine's class items, named rules, word boundaries, `<ws>` and the
first-set prefilter all use it. The prefilter no longer treats `\d` as ASCII only.
`t/regex/regex-cclass-parity.t` checks each class against `nqp::iscclass` over a codepoint sample,
along with 15 rows measured with rakudo.
