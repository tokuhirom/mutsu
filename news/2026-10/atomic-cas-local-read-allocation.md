# Reduce allocations in atomic CAS local reads

An atomic scalar read now uses its existing environment name directly when the name already identifies the binding. The VM checks an atomic type constraint through its symbol and stored value, and constructs the legacy atomic lane key only when that lookup needs it. CAS assignment also borrows the stored type constraint and reuses the target symbol for its readonly check.

These changes remove per-iteration string and value allocations from the common typed `atomicint` retry loop while preserving the name lookup used for aliases and other nonlocal targets.
