# Replacement-block exceptions propagate through `.subst`

`Str.subst` now propagates exceptions raised by a replacement block instead of
silently replacing the match with `Nil`. This lets real-world callers such as
`ComfyUI::API` reject missing workflow variables as intended.

Pinned by `t/regex/subst/subst-replacement-exception-propagates.t`.
