# A closure call stops allocating for its own bookkeeping

Calling a closure through a code variable (`c()`, `&code()`) paid for several
pieces of bookkeeping on every call that never depended on the call:

- the parser's callsite-line marker (`__mutsu_test_callsite_line => LINE`) was
  built with `MakeNamedArg` at run time; it is now one constant Pair;
- stripping that marker rebuilt the argument `Vec`, cloning every other
  argument; it is now removed in place;
- `routine_current_view` allocated an `Arc` before asking whether the routine
  was ever mixed into, which nearly none is;
- the multi-dispatcher probe ran twice per call through a string-keyed env
  lookup; it runs once, by symbol;
- the caller-frame record (`CallFrameEntry`) resolved its file name to a
  `String` per push; it holds the interned `Symbol`.

Callgrind, per call, profiling build:

| call | before | after |
| --- | ---: | ---: |
| `my &c = { 1 }; c()` | 12.2k Ir | 10.2k Ir |
| `sub b(&code) { code() }` | 12.0k Ir | 10.2k Ir |
| `my $c = { 1 }; $c()` | 10.1k Ir | 9.2k Ir |
| `{ $_ + 1 }` called with an argument | 13.4k Ir | 12.4k Ir |
| pointy `-> $x { $x }` (light path) | 4.8k Ir | 4.3k Ir |
| closure writing an outer variable | 22.4k Ir | 21.3k Ir |

AttrX::Mooish's per-iteration loop moves from 7.73M to 7.56M Ir. ADR-15737 D1,
refs #15737.
