# The default rendering behind a user gist/Str/raku is a deferral entry

`callsame`/`nextsame` out of a user `gist`, `Str` or `raku` reaches the default rendering through a
`DeferralEntry::Native` of the frame instead of a probe after the user candidates (ADR-11276 slice 4).
