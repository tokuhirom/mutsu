Fix qualified type lookup in a module's own routines after a separate module has
published the namespace root. This unblocks `Zef::Identity` when `Zef::CLI`
loads `Zef` before calling `str2identity`.
