# `m:i/$re/` sees a lexical that nothing else has published

A regex literal with adverbs (`m:i/^$re/`, `m:g/$x/`, `m:s/$x/`) resolves its
interpolated variables by name. The pre-match publish of the frame's slots into
`env` skipped a local that had no `env` entry yet, so `my $re = "met"; "meter" ~~
m:i/^$re/` saw an undeclared variable (the always-failing `<!>`) until some other
match happened to publish it. The publish now also covers a slot that already
holds a value, and still leaves an unset slot of a later scope alone.

Geo::Ellipsoid (`$unit ~~ m:i/^$re/` in `set_distance_unit`) goes from 0 to
raku's ok count on all 11 test files.
