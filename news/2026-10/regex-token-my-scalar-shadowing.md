# Regex token `:my` scalars survive repeated matching

Token pattern interpolation now leaves `:my` declarations and their later
references for match-time handling. A same-named scalar in the caller, or a
scalar left in the environment by an earlier match, no longer gets spliced into
the declaration before the token is parsed.
