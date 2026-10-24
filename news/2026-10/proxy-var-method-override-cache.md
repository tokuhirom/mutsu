# The native-override answer is cached whole

`native_lever_a_user_override_sym`, asked on every method call whose
receiver type has a native row, cached only whether a user method of that
name exists and then re-ran the outranking check (an MRO walk, ~12k
instructions) on every call. Both halves depend on the receiver type, the
method name and the registry alone, so the cache now holds the combined
answer. `$proxy.VAR.is-set` on a Proxy subclass drops from about 80k to about
69k instructions per call; the rest is the method-call frame (#14855).
