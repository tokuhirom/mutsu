# `Ambiguous call` lists each tied candidate as `(signature) from FILE line N`

Found by the raku-doc differential sweep (`Language/signatures.rakudoc`). The message of a
sub's `X::Multi::Ambiguous` printed `Signature<31>, Signature<33>` on one line and left the
quotes off the call; it now matches rakudo: `Ambiguous call to 'baz(Int)'; these signatures
all match:` followed by one `  (Int $i) from FILE line N` line per candidate.
