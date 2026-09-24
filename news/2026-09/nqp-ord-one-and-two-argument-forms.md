# nqp::ord gains its one- and two-argument forms; #9203 closed

Issue #9203 reported that the whole `nqp::` string family indexed strings by
codepoint while MoarVM indexes them by NFG grapheme, so `nqp::chars`,
`nqp::ordat`, `nqp::substr`, `nqp::index` and friends disagreed with rakudo on
a combining mark that has no NFC composite (`"a\x[20DD]b"`) and on `"\r\n"`.

By the time the issue was picked up, the core of it had already landed with
the ADR-0117 refactor (`str-methods-and-nqp-ops-share-one-routine.md`): every
`nqp::` string op, the TRIR typed string ops and the `Str` methods now call the
same grapheme-indexed routines in `src/builtins/str_prim/`, and the private
`Vec<char>` memo (`nqp_char_cache`) is gone. Re-running the issue's repro on
`main` gives rakudo's answer on every line, in both the interpreted and the
JIT-compiled (TRIR) paths.

The one gap left was the issue's side note: `nqp::ord` did not exist at all,
in either form (`Unsupported nqp:: op: nqp::ord`). It is now the same op as
`nqp::ordat` with the position defaulting to 0, so it shares its routine
(`str_prim::nqp_ordat`) and its semantics, all measured against rakudo:

| call | result |
|---|---|
| `nqp::ord("a\x[20DD]b")` | 97 |
| `nqp::ord("a\x[20DD]b", 1)` | 98 |
| `nqp::ord("x\r\n", 1)` | 13 |
| `nqp::ord("")`, `nqp::ord("ab", 5)`, `nqp::ord("ab", -1)` | -1 |

`t/vm/nqp-str-prim-parity.t` pins these, plus agreement with `nqp::ordat`; the
file also passes unchanged under rakudo.
