# Stores read a slot's type constraint from the slot

The local-variable store paths (`SetLocal`, `AssignExprLocal`, the element and
inc/dec fast paths, the atomic read) now ask `slot_type_constraint*` and the new
`slot_hash_key_constraint` instead of walking the env by name. A slot the code
declares reads only the running frame's record, so a caller's same-named typed
variable no longer answers for it (ADR-13757 section 2.2, #14226).
