# The sprintf directive-count error message is word-wrapped as a whole

`X::Str::Sprintf::Directives::Count` always broke the line after "was" and left
the echoed format on a long second line. The message is now word-wrapped as a
whole, so the format and the interpolated-`$` hint wrap as in rakudo (#16854;
found by the raku-doc sweep, `Type/independent-routines.rakudoc`).
