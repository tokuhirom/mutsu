# Resolved: pointy-block custom-trait "unknown trait" flip on Cro::HTTP::Router

`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`
tracked `use Cro::HTTP::Router` intermittently failing "Can't use unknown
trait 'is' -> 'query'" in a parameter declaration — the pass rate for
`http-router.rakutest` had flipped unpredictably between builds (as low as
0/83, as high as 82/83) across many prior sessions. Two theories
(`ParseMemo` pointer-identity cache collision; module-scan truncation) were
investigated at length and both ruled out without finding the actual cause
(see the ticket's full investigation history in git log for this file's
prior path).

The real root cause was found this session, while probing the "genuine
miss" investigation path the ticket left open: a parser bug in the
colonless "compact" match-adverb parser
(`news/2026-08/compact-match-adverb-overreach-mis-parses-bareword.md`) that
mis-parsed ordinary bareword method-call chains (`msg.gist // msg.gist`,
exactly the shape used by `Cro.rakumod`'s `whenever $in -> \msg { ... }`
transform) as bogus regex literals — corrupting the parse of `Cro.rakumod`
itself (transitively pulled in by `use Cro::HTTP::Router`) in a way that
could silently drop the `trait_mod:<is>` export registration the "unknown
trait" symptom was chasing.

**Result after the fix**: `http-router.rakutest` is now **439/439**
(previously capped at 82/83 even on its best runs). Full Cro::HTTP suite
re-measured at **32/35 fully-green files** (up from 31/34 at the last
reliable measurement), Cro::Core remains **9/9**. Verified stable across 5
repeated runs and across rebuilds with deliberately perturbed binary layout
— the historical flip no longer reproduces.
