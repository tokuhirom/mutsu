# ADR-0130: A test file that fails only on a Rakudo artefact mutsu will not copy is graded `accepted`

- Status: Accepted (implemented)
- Date: 2026-09-27
- Refines: ADR-0085 (ecosystem test-suite parity measurement), D2 and D3
- Resolves: [#9746](https://github.com/tokuhirom/mutsu/issues/9746)

## Context

ADR-0085 makes rakudo the denominator: a test file counts toward the KPI when
rakudo passes it, and mutsu failing it is a `partial` / `regression` — the
actionable buckets every "what to fix next" tool draws from
(`ecosystem-dist-roulette`'s `pick-dist.py`, `ecosystem-tickets.py`, the site
manifest).

That assumes rakudo's pass means "the language requires this". Sometimes it
does not. #9746 is the first case: DSL::Shared 0.2.11's
`t/Array-of-regexes-matches.rakutest` test 6 passes on rakudo only because
rakudo caches a Str interpolated as `/<$rx>/` by mixing
`Match::CachedCompiledRegex` into that Str object **in place**. That changes its
`.WHICH`, so a later `'string' (elem) @knownRegexes` is False, and the code
under test takes a different path. Neither roast nor raku-doc specifies this.
Reproducing it would mean giving mutsu's value-typed Str an object identity and
an in-place rebless, and sharing Str literal constants across calls. That is a
value-model change that would copy a cache artefact. The user decided not to
reproduce it (2026-09-27).

Recording the decision in the issue alone is not enough. The ledger would keep
the file `partial`, and the next random draw would reinvestigate it from
scratch. The only existing escape hatch, `ecosystem/exclude.txt`, works per
distribution. DSL::Shared has another, genuinely fixable failing file
(`t/Entity-names-parsing-via-resources-access-object.rakutest`), and excluding
the distribution would hide it.

## Decision

1. **A per-file accept list, `ecosystem/accepted-divergences.toml`.** Each
   `[[divergence]]` entry names a `dist`, a `file`, the `issue` that records
   the decision, a `reason`, and the exact `shape` of mutsu's failure: a subset
   of the record's `mutsu` side (`verdict` and `first_failure` required; `ok`,
   `nok`, `plan` optional).
2. **A new `cmp` value, `accepted`.** `ecosystem-sweep.py` grades a file
   `accepted` when compare() would call it `partial` or `regression` and the
   mutsu side matches an entry's shape exactly. An `accepted` file is out of the
   denominator, like `no_baseline`: it is neither parity nor a mutsu failure.
   The rollup reports the count as `accepted_files`, so the size of what has
   been set aside stays visible.
3. **Exact shape, so an entry cannot hide a new bug.** A file that starts
   failing any other way — another assertion, one more failure, a die — is
   graded normally again and comes back into every tool's pool. Once mutsu
   passes the file, it is `parity`, and the stale entry is visibly dead.
4. **`--regrade` applies the list without measuring.** Grading is a pure
   function of the stored sides plus the list, so the PR that adds an entry
   also regrades the record. The ledger then reflects the decision right away,
   instead of waiting for the next sweep of that distribution.
5. **Admission is a decision, not a judgement call.** An entry needs an issue
   recording a decision `AGENTS.md` reserves for the user, or an ADR's, that
   mutsu will not copy the behaviour. "Looks hard" never qualifies.
   `exclude.txt` remains the tool for a whole distribution that can never pass.

## Consequences

- The random draw and the tickets report stop offering a file that has already
  been decided. Every other file in the same distribution stays measured and
  drawable.
- The KPI's denominator shrinks by the accepted files. That is honest in the
  same way `no_baseline` is: rakudo's pass there is not a compatibility claim
  mutsu is failing. `accepted_files` in `summary.json` / `summary.md` keeps the
  count in the open.
- A distribution whose only remaining failures are accepted reads `green`. The
  record's `files[]` still shows which files are `accepted`, and why is one
  lookup away in the list.

## Alternatives considered

- **Reproduce the mixin (#9746 option B).** Rejected by the user: it is a
  value-model change (Str identity, in-place rebless, shared literal constants)
  that copies an implementation artefact, for one assertion in one
  distribution.
- **Add the distribution to `exclude.txt`.** That hides DSL::Shared's other,
  fixable failure, and every future regression in the distribution.
- **Accept a file regardless of how it fails.** That is simpler, but an entry
  would then hide any later bug in that file indefinitely. The exact shape is
  what makes the list safe to trust, for the same reason as
  `ci/known-env-failures.toml` (ADR-0126 §2.6).
