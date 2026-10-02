# `@GLOBAL::d = ...` and `.push` reach a top-level `our @d`

`our @d; @GLOBAL::d = 1, 2; say @d` printed `[]`: a whole-variable store
through the pseudo-package-qualified name minted a second container under the
`@GLOBAL::d` key, so the bare `@d` (and a later `@GLOBAL::d.push`, which then
found that second container) never saw it. The same held for `%GLOBAL::e`.

`SetGlobal` now resolves such a name to the container its bare `our`
declaration holds (`package_container_store_name`, sharing the key rule the
element-write prologue already used), so list assignment writes into the
declared container. An undeclared qualified slot still item-assigns into
Rakudo's auto-created Scalar (`$(1, 2)`).
