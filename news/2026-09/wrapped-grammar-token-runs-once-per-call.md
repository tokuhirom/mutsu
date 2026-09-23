# A wrapped grammar token's wrapper runs once per call

`Grammar.^find_method('word').wrap(...)` installs a wrapper that the regex
engine re-enters on every `<word>` call. For `token TOP { <word>+ % ' ' }`
parsing `'ab cd ef'`, mutsu ran the wrapper six times where Rakudo runs it
three times (#9151). The wrapper is user code, so a wrapper that counts or logs
could see the difference.

The count came out right only by coincidence in other shapes, because three
separate paths disagreed about when a wrapper runs:

- **The LTM prefix measurement ran it.** `Grammar.parse` ranks the start
  rule's candidates with `ltm_prefix_len_at`. With no code atom in the pattern,
  that is a full match of the rule, and the named-subrule arm deliberately let
  a wrapped token escape the declarative-mode rules and dispatch its wrapper.
  Rakudo's NFA measures a wrapped token by its own body and never enters the
  wrapper. `token_method_wrap_chain` now reports no chain while
  `LTM_DECLARATIVE_MODE` is set. The same applies to `CODE_ATOMS_INERT`, the
  failure-position probe that re-matches the pattern after a failed `.parse`,
  which ran the wrapper a second time for a failing parse.
- **The streamed subrule driver never ran it.** A plain `<w>` call in a rule
  goes through `drive_named_subrule_candidates` (ADR-0073), which matched the
  token body directly and never consulted the wrap chain. `rule TOP { <w> <w> }`
  with a wrapped `ws` therefore ran the wrapper only during measurement. The
  count matched Rakudo's 2, but only because the measurement pass happened to
  make the calls. The driver now declines (`StreamDecline::MethodWrapInstalled`)
  whenever a method wrap is installed anywhere. That covers a wrapped token.
  It also covers the per-subrule routine frame the eager arm pushes so a
  wrapper can name its calling rule from a `Backtrace`, which a stream cannot
  keep scoped to the call. Grammar::PrettyErrors' `lastrule` depends on that
  frame.
- The now-unreachable "wrapped token escapes LTM mode" guard in the atom
  matcher was removed.

`t/grammar/grammar-wrapped-token-dispatch.t` now asserts exact counts, and they
match Rakudo 2026.07. It covers the separated quantifier (3), a failing parse
(3), a wrapped `ws` under a `rule` (2), a token called from ranked proto
candidates (1), and the calling rule's name as seen by the wrapper.
Grammar::PrettyErrors' own suite stays 24/24.

Still open: Rakudo does not run a wrap installed *after* the grammar has
already parsed once, and mutsu does.
