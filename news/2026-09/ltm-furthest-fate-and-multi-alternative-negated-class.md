# LTM measures the furthest fate; multi-alternative negated classes and qualified subrule calls are fates

`CSS::Module::CSS3::Selectors` 0.0.6 reported `bad :not() argument: p` for `:not(p)`
([#9053](https://github.com/tokuhirom/mutsu/issues/9053)). Its `negation-expr` rule is
`[<qname> | <universal> | ... | <any-arg> ]+`. On `p`, Rakudo takes `<qname>` and mutsu took
the catch-all `<any-arg>`. The issue had been investigated twice without finding the cause. One
theory was that Rakudo's subrule cycle guard (`%seen` in `NFA.nqp::mergesubrule`) is shared
across sibling branches. NQP's source clones that hash per call, so the theory is wrong.

Three separate differences from Rakudo caused the result. All three had to be fixed.

**1. `<-[Z \n]>` is not declarative in Rakudo.** NQP's `cclass_elem` action folds the plain
entries of a `[...]` enumeration into one `enumcharlist`. Every range, class escape, negated
escape and `\n` (which also matches `\r\n`) becomes an alternative of its own. When a negated
class has two or more alternatives, it compiles to `[<?conj> .]`, and the NFA cannot build a
`conj`. The class therefore ends the declarative prefix. CSS::Grammar's `nonascii` is exactly
such a class, and it is reached from `nmchar`, which is repeated under `Id`. So both `qname` and
`any-arg` measure 1 on `p`. They tie, and declaration order picks `qname`. mutsu treated the
class as declarative, so it measured `qname` as 0 (its sigspace `<.ws>` stopped it) and
`any-arg` as 1. This PR checked the rule against `raku` for over twenty class shapes. `<-[Z \n]>`,
`<-[a..c x]>`, `<-[\d x]>` and `<-[\n \r]>` terminate the prefix. `<-[\n]>`, `<-[a..c]>`,
`<-[\t \r x]>` and `<-[\x0a x]>` do not: a hex escape is a plain entry, while `\n` is not.

**2. mutsu stopped at the first fate instead of the furthest one.** A stopper (`<.ws>`, a code
block, now also that class) used to succeed as a zero-width match and unwind the whole walk.
The measured prefix was then wherever the depth-first walk met its first stopper. Rakudo's NFA
advances every path together, a fate ends only its own path, and the prefix is the furthest
fate. The reduction also depends on this: `qname` reaches offset 2 on `pq` only through a fate
inside the optional `namespace-prefix`.

A stopper now records its position and fails its own path, and the walk carries on
(`regex/regex_ltm_fate.rs`). Measurement entry points open a frame and take
`max(furthest full match, furthest fate)`. The matchers that walk a re-sliced or transformed
subject (`:m`, `:i` case folding, the no-capture subrule prober) map their fates back. A
measurement no longer scans past start 0. The ratchet fast paths are skipped while measuring,
because the NFA ignores `:ratchet` and those loops never try the atom at the end of the subject.
The per-sibling flag resets that #9087 added are no longer needed and are gone.
[ADR-0111](../../docs/adr/0111-ltm-stoppers-end-one-path.md) records the decision as an
amendment to ADR-0022 §4.2.

**3. A package-qualified subrule call is a fate in Rakudo.** In the real grammar, `any-arg` is
`rule {<CSS::Grammar::Core::_arg>}`. The NFA looks `CSS::Grammar::Core::_arg` up as a method
of the cursor, finds none, and puts a fate there. That happens even for `<G::x>` naming the
grammar itself. So `any-arg` has prefix 0 and `qname` wins outright. mutsu measured through the
call. With all three fixes the distribution's `t/00basic.t` goes from 6 failures to 1. The last
one, assertion 21, has an unrelated cause: a `proto rule` candidate swallows trailing whitespace.
That is filed separately as [#9094](https://github.com/tokuhirom/mutsu/issues/9094).

Pinned by `t/regex/regex-ltm-furthest-fate.t`: the issue's reduction, an ordering probe that
needs the furthest-fate rule, the class shapes above, and qualified calls. Uniprop atoms (`<:L>`) and `<-alpha>`
are also fates in Rakudo and are still measured as declarative here. The ADR lists them as known
gaps.
