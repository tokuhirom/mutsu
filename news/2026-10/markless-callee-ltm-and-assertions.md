# A markless `:m` callee covers `|` and zero-width assertions

A whole-`:m` rule called over a subject with combining marks runs as a markless frame (#7548):
each consuming atom compares a grapheme's base. Until now, a body that held an LTM alternation
(`[a | b]+`) or a zero-width assertion (`<<`, `>>`, `<|w>`) still took the eager path, so its code
blocks ran once per end computed rather than once per end entered.

Both cases are now handled in the markless frame. They answer from the mark-stripped view of the
subject, which is the published match target's cached one when the subject is that target. An
alternation ranks its branches there; only their order is used, so no positions need mapping
back. An assertion is decided at the mapped position, so a word boundary never falls between a
letter and its mark. Look-arounds and backreferences in such a body still take the eager path
(#13176).
