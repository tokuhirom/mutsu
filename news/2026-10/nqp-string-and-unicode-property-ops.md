# `nqp::` string and Unicode-property ops

Part of the `nqp::` coverage campaign (#11488): the 28 string and
Unicode-property ops tracked by #11495 used to die with
`Unsupported nqp:: op`. The String and Unicode Properties categories of
`docs/nqp-op-coverage.md` are now complete (393 of 577 documented ops).

Each op runs the routine its Raku spelling already runs (ADR-0117):

- `fc`, `tclc` and `codes` are the `Str` methods' bodies. `tc` titlecases
  *every* grapheme (`nqp::tc("ßa")` is `SsA`), with the same per-grapheme
  mapping `Str.tc` uses for its first one.
- `indexfrom`, `rindexfrom`, `substr_s`, `ordfirst` and `ordbaseat` are the
  grapheme-indexed `str_prim` routines. `replace` is MoarVM's
  `substr($s, 0, $from) ~ $with ~ substr($s, $from + $count)`, quirks
  included (`nqp::replace("abc", -1, 0, "X")` is `abcXc`).
- `sprintf` runs the formatter and the argument checks of `sprintf` and
  `.fmt`; `sprintfdirectives` counts the arguments a format takes in order.
- `unicmp_s` is `coll`'s collator, under the collation-level word it is
  passed.
- `encode` appends to the buffer through `Str.encode`'s encoder;
  `decodetocodes` uses `nqp::decode`'s decoder. `normalizecodes`,
  `encodefromcodes` and `decodetocodes` share the codepoint-array helpers
  with `strtocodes` and `strfromcodes`.
- `radix_I` shares `nqp::radix`'s digit scanner and accumulates into a big
  integer, so it never wraps.

The property ops answer with MoarVM's own numbers, read off rakudo, because
nqp code compares them as plain integers. `unipropcode` now knows Script and
the 60 binary properties mutsu can evaluate, as well as General_Category;
`unipvalcode` takes long names and short aliases (`Latin`, `Latn`, `latin`).
`getuniname`, `codepointfromname` and `strfromname` use the name tables
behind `uniname`, `\c[...]` and `uniparse`. `codepointfromname` accepts only
the exact spelling, as MoarVM does.

One divergence was fixed on the way: `nqp::substr` with a length below -1
now dies "Substring length (-2) cannot be negative", as it does in MoarVM.
Before, mutsu read any negative length as "to the end".
