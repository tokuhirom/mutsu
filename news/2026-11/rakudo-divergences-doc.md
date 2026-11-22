# One page lists every deliberate divergence from rakudo

`docs/rakudo-divergences.md` collects the places where mutsu deliberately behaves
differently from rakudo 2026.11 and keeps doing so. Each entry gives a minimal
repro with both outputs where one fits, the reason, and links to the issue, ADR
or PR that records the decision. The reasons fall into five groups:

- security and robustness;
- roast or raku-doc chosen over rakudo;
- rakudo artefacts and quirks not copied;
- implementation identity;
- the ecosystem test files graded `accepted` under ADR-0130.

It includes the maintainer's decision on #18729: a block's operator `multi` sees
its scope's outer lexicals in mutsu (`5 10 15`). rakudo instead gives `0 0 0`
with warnings, because its derived operator dispatcher holds the uncloned
candidate.

From now on, a PR that makes a new deliberate divergence adds it to that page.
