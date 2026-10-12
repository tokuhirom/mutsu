# A wrapped token's `callsame` result is the caller's capture

With `G.^find_method('num').wrap: sub (|c) { my $r := callsame; @seen.push: $r; $r }`, rakudo
files the Match the wrapper returns as the caller's `$<num>`, so `$m<num> === @seen[0]`. mutsu
filed a capture node rebuilt from the token's captures, a different Match. The wrapped subrule
call now seeds the filed node's lazy-Match memo (ADR-11404) with the returned Match whenever the
two read exactly alike -- same span and metadata, the very same child nodes -- so both are one
object with one identity (#12918).
