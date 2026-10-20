# A native method's Signature is built once per owner

Each `C.^lookup('new')` / `Str.^find_method('chars')` synthesized a fresh `Signature` that stayed in the
process-global signature registry forever (~600 B per call). The signature of a native method
depends only on its owner, so it is now memoized per owner (Rakudo returns the same `Method`
for repeated lookups). The native-signature loop of #13009 now retains ~0 B/iteration.
