# Grammar parsing no longer pays for token `.wrap` support when nothing is wrapped

The bench history showed `bench-grammar-parse-big` jump at `b07f4758`
(Grammar::PrettyErrors support) from 338M to 788M instructions (2.33x) and from
258K to 1.91M heap allocations (7.4x), and stay there.

Two additions from that commit ran on every grammar subrule, whether or not any
method had been wrapped:

- `token_method_wrap_chain()` was asked about every named atom and `<.ws>` the
  regex engine visited (in `walk_tokens`, `regex_match_atom` and the simple
  atom matcher). Each call cloned the receiver's whole MRO into `String`s and
  built two more `String` registry keys per owner. That was the allocation
  blow-up.
- `subrule_candidate_ends_with_frame()` pushed and popped a routine frame
  around every subrule candidate (20k per run), so that a wrapped token could
  see its enclosing rule in a `Backtrace`.

Both now return early when the method wrap table is empty, which is a single
`is_empty()` check. With a wrap installed, behaviour is unchanged.

Measured locally with `scripts/bench-det.sh` (release build):

| benchmark | before | after |
|---|---|---|
| bench-grammar-parse-big instructions | 788M | 351M |
| bench-grammar-parse-big allocations | 1,913,602 | 258,840 |

The allocation count is back to the pre-regression 258K. The remaining
instruction gap against the 338M CI baseline is within the difference between
the local toolchain and CI's, plus the farthest-position recording that the same
commit added for parse-failure diagnostics.

Pinned by `t/grammar/grammar-wrapped-token-dispatch.t`, which checks that the
first wrap installed after a wrap-free parse still takes effect.

Still open: mutsu runs a token's wrapper twice per match where Rakudo runs it
once ([#9151](https://github.com/tokuhirom/mutsu/issues/9151)).
