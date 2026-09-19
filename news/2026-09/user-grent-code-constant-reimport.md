# Code-valued exports survive a tagged re-import

Module-scope `my constant &name` exports now retain their code values after a
first scoped import. A later import with another export tag can recover the
same aliases instead of silently omitting them. This makes `User::grent`'s
`:FIELDS` imports of `setgrent` and `endgrent` visible as Rakudo does.

Pinned by `t/modules/import-export/exported-code-constant-reimport.t`.
