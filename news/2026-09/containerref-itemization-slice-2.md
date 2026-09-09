# Shared scalar holders retain itemization locally

ADR-0079 Slice 2 now gives `$`-share targets an itemized
`ContainerRefItemized` word while leaving the source `@`/`%` holder plain over
the same cell. Dereference, array-element reads, and hash initialization honor
that word-local flavour, so a shared scalar renders as `$[...]` or `${...}`
without changing the aggregate source's rendering or flattening behavior.

Plain scalar parameters that share an array or hash container use the same
itemized target holder. Focused regressions cover array/hash shares, scalar
parameter binding, source-holder preservation, and the existing odd-number
hash-initializer guard.
