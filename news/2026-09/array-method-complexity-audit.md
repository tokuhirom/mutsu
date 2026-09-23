# Array / List operations now document their complexity, and the quadratic ones are tracked

This is the third audit in the series, after the `nqp::` ops
(`news/2026-09/nqp-op-complexity-audit.md`) and the `Str` methods
(`news/2026-09/str-method-complexity-audit.md`). Array, List and Seq
operations now carry `// Cost:` lines. That covers mutation (push, pop,
shift, unshift, splice, element store, shaped arrays), traversal (map, grep,
first, head, tail, sort, unique, reduce, `for`, `Z`, `X`), and search,
random, set coercion and stringification. When mutsu's bound is worse than
Rakudo's, the line adds `Rakudo: O(..) -- see #NNNN`. Together with the Str
audit, `grep -rn 'Rakudo: O(' src/` now finds 130 deficit sites. The format
is defined in `docs/complexity-annotations.md`.

`scripts/array-complexity-check.sh` times 33 cases at N and 2N. Many deficits
here are O(e) per call where Rakudo is O(1), so most cases time a fixed
number of calls against an N-element array. Each case therefore records the
ratio a healthy implementation would show, and a case is flagged only when
its measured ratio exceeds that. Like the other two scripts, it is a manual
diagnostic and not a CI gate.

## What the audit found

All measurements are from a release build of current `main`. Current `main`
includes #9121, the head-offset fix that made `shift` loops and
single-element `unshift` loops linear; both stay in the script as controls.

- **#9156: front and middle mutation that #9121 did not cover.**
  - Any write after a `shift` compacts the array first, so a push+shift
    queue pays O(e) per step.
  - A multi-element `prepend` costs O(k·e), and `%h<k>.unshift` is
    quadratic in a loop (ratio 3.7 to 4.1).
  - `splice` costs O(e + r·(e−s)): ratio 3.6 in a loop, 4.3 for one call.
- **#9157: `.ASSIGN-POS` / `.BIND-POS` called as methods, and shaped-array
  element stores, are O(e) per call.** The first copy the whole array; the
  second re-validate the shape on every store (ratio 3.9 to 4.1).
- **#9158: finite `map`/`grep` pipelines are eager.**
  - `.map(...).head(3)` runs every callback.
  - `for @a { last }` copies the array.
  - `.combinations(2).head(10)` builds every pair (ratio 5.1).
- **#9161: `[~] @a` is quadratic** (ratio 4.1; 15.9 s against Rakudo's
  0.78 s at 200k elements). **`.unique` on Rats, Pairs or objects is
  O(e²)** (ratio 4.0).
- **#9162: many methods copy the whole invocant to answer a small
  question.** Affected: `.first`, `.pick`, `.tail`, `.skip`, `eqv` on
  unequal lengths, `==`, `.gist`, `.List` and `.cache`. Each is O(e) per
  call where Rakudo is O(1), O(k) or O(i).

The audit also found correctness bugs:

- **#9159 (`todo:deep`): laziness is lost on infinite input.**
  - `(1..*).map(*+1).skip(2).head(2)` returns `()`.
  - `rotor`, `batch` and `unique` throw "Cannot .X a lazy list".
  - `Z` and `X` silently cap at 1000 and 256 elements.
  - `roundrobin((1..*), ...)` panics with a capacity overflow.
- **#9160 (`todo:ticket`):**
  - `zip()` truncates finite lists to 1000 rows.
  - `say @a` does not stop at 100 elements the way `@a.gist` does.
