# A native deferral entry names its bridge

`callsame` / `nextsame` reaching the native end of a method chain (a grammar `parse`, a user `new`, a metamodel HOW method, a
container subclass's storage, ...) used to probe four bridges in turn by the dynamic call-context name. The frame builder now
records which bridge applies in `DeferralEntry::Native { base: NativeBase }`, and the advance arm matches on it. A core-type
`augment` of a method named like a bridge (`parse`, `new`) no longer reaches the wrong one. ADR-11276 §9.53; closes #12423.
