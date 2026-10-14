# A `:my $*x` written by a code block reaches the sub-rules of a nested rule

A grammar rule that declares `:my $*x` and writes it in a code block now shows
the written value to the sub-rules it calls even when the rule itself was
reached through another rule (`token comment { <dc> }`). The code block's write
was kept only in the capture delta and the env was restored right after the
block, so a callee read the stale value. An uninitialized `:my $*x;` is now also
recorded in the delta so its first write is harvested. Found with Rainbow's
delimited comments (#13222).
