# A too-permissive colonless "compact" match adverb parser mis-parsed ordinary barewords as regex literals

`ms/pattern/` is the one colonless compact match adverb Raku recognizes — a
single `s` directly after `m`, shorthand for `m:s/pattern/` (sigspace).
Every other letter or combination is a syntax error in real Raku:
`raku -e 'mi/x/'`, `mg/x/`, `mp5/x/`, `mss/x/` all die "Missing required
term after infix".

mutsu's `parse_compact_match_adverbs` did not know this: it greedily
consumed ANY run of the letters `s`/`i`/`g`/`m`/`p`/`c` (plus a colonless
`p5`) immediately after a bareword/identifier starting with `m`, then
treated the next character as a regex delimiter and scanned the rest of the
buffer for a matching close. An ordinary method-call chain on a sigilless
bareword — `msg.gist // msg.gist` (a `whenever $in -> \msg { ... }` body is
exactly this shape) — satisfied the pattern: `m` + `s` + `g` consumed as
three "compact adverbs", `.` treated as the delimiter, and the `.` in the
*second* `msg.gist` supplied a spurious matching close, silently
mis-parsing the whole expression as a bogus regex literal instead of two
method calls joined by `//` (defined-or).

This was the real root cause of a symptom that had haunted the Cro campaign
for weeks: `use Cro::HTTP::Router` intermittently failing "Can't use
unknown trait 'is' -> 'query'" depending on the exact build
(`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`).
Cro's own `Cro.rakumod` (`PipelineTraceTransform.transformer`) contains
`whenever $in -> \msg { my $output = (try msg.trace-output) // msg.perl; ...
}` — exactly the mis-parsing shape — so `use Cro::HTTP::Router` (which pulls
in `Cro.rakumod` transitively) could fail to parse at all, corrupting
whatever came after and, depending on exactly where the corruption landed,
sometimes losing the `trait_mod:<is>` export registration the ticket's
"unknown trait" symptom was chasing. Two independent theories (ParseMemo
pointer-collision, module-scan truncation) had been investigated and ruled
out across several prior sessions without finding this.

Fixed by restricting `parse_compact_match_adverbs` to the one real form: a
single leading `s`, nothing else. Verified against `raku -e` for every
removed letter/combination, and against `roast/S05-modifier/sigspace.t` /
`t/regex-sigspace.t` (the `ms//` sigspace shorthand this function exists
for) to confirm no regression. `make test` clean. The original ticket's
exact minimal repro now passes stably across five consecutive runs and
across rebuilds with deliberately perturbed binary layout (previously it
flipped between working and "unknown trait" unpredictably).

Pin: `t/regex-compact-adverb-bareword-misparse.t`.
