# Seq state checks belong to method rows

The zero-argument native dispatch entry no longer matches `cache`, `is-lazy`
and `kv` to inspect Seq state before dispatch. Cache requests already belong
to the cache row. Seq and Mu lazy-query rows now share the consumed-body
refusal, and Any's kv row preserves the deferred indexed IO stream produced
by the VM's consumption step. Calling a row directly therefore keeps the
same state semantics as an ordinary method call.

Focused oracle tests cover cache reuse, non-consuming lazy queries, consumed
Seq errors and streaming line/word indices. This advances campaign #11276.
