# STORE of the quant hashes is a method-table row

`STORE` on `SetHash`, `BagHash`, `MixHash`, `Set`, `Bag` and `Mix` is now a single mutating row. The `nextsame` bridge of
`is BagHash` subclasses, the subclass delegate in the VM and the by-value entry no longer carry their own copies of the
folding logic (ADR-11276 §9.40, part of #12387).
