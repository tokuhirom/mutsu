# A method call with named arguments takes the plain-method lane

`$c.m(1, :flag($t))` on a user class walked the whole receiver-probe prefix of
`CallMethodMut` on every call, and a `--> Int` return type re-resolved the spec
and re-ran the general return check on every call. Three levers, measured on
the `profiling` build with callgrind (slope of 500 vs 2500 iterations, second
run, same checkout, A/B per lever) for
`method m(Int $x, Bool :$flag = True --> Int)` against the positional-only
`method m0(Int $x)`:

| | `m` Ir per call | `m0` | ratio |
| --- | ---: | ---: | ---: |
| before | 31.2k | 15.6k | 2.0x |
| built-in-outranks probe memoized per (class, method) | 29.1k | 15.6k | 1.87x |
| plain core return type checked without resolving the spec | 24.7k | 15.6k | 1.58x |
| named arguments keyed into the plain-method lane | 21.3k | 15.65k | 1.36x |

- `user_candidates_all_outranked_by_builtin` walked the MRO with a built-in
  row probe per level on every call; its answer reads only the MRO, the
  candidate lists and the static row table, so it joins the registry-generation
  probe memos.
- A `--> Int` that names itself for the owner (memoized per owner and spec,
  off once any `::T` capture exists) is checked against an `Ok` value directly;
  an error outcome, a container, a Proxy or a refused value takes the general
  path unchanged.
- The plain-method lane's key now holds each named argument's name and its
  value's type key (the form the method candidate caches already use); a
  pair-valued argument or a Junction value still refuses the lane.

This is a slice of #15443 (close condition for the method twin: within 1.3x of
`$c.m0(1)`).
