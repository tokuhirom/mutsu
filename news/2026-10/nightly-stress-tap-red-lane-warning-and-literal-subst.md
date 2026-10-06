# Nightly stress TAP red: a debug-only lane check and a quadratic literal `.subst`

The nightly `gc-stress-tap` and `jit-stress-tap` jobs ([#11979](https://github.com/tokuhirom/mutsu/issues/11979))
failed on the same two files and nothing else (no crash report, no
`VERIFY FAIL`). Both were deterministic bugs, not flakes: they reproduce on a
plain debug build with no GC or JIT setting.

- `t/exceptions/typed-exception-attributes.t` aborted with "the method-table
  lane disagrees with the full CallMethodMut path for .contains". That check
  (`check_method_site_lane`, debug builds only) compared the lane's answer
  before the native warning was settled with the full path's answer after it.
  `List.contains` / `.index` / `.rindex` and the `Map`/`Hash` forms answer with
  a resumable warning, so every such call on a variable tripped it. The check
  now compares the value the warning resumes with. Release builds were never
  affected: `finish_method_site_lane` already settled the warning.
- `t/regex/subst/subst-slow-path-linear.t` timed out on its last case. The
  literal `.subst` slow path (closure replacement, `:x(*)`, `:nth`) publishes
  `$/` through bare-span captures that carry no subject, and
  `subst_match_var` built a whole-subject `MatchTarget` for each of them, twice
  per call: O(n x matches), the #9143 / #8247 shape coming back. They now share
  one. `("a," x 8000).subst(",", { "" }, :g)` took 8 s on a debug build; the
  match-target counter for 1500 literal matches dropped from 3003 to a handful.

Pins: `t/oo/method/method-table-lane-list-search-warning.t` (run under the
debug lane check) and `literal_subst_slow_path_publishes_one_subject_for_every_match`
in `tests/regex_subject_materialized_once.rs`.
