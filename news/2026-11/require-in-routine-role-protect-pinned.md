# A required module's lexicals inside `Lock.protect`, reached through a role, are pinned

#18126 reported a second shape of the Monad regression: a module loaded by a
`require` inside a sub, whose sub runs a `Lock.protect` block that reads the
module's file-scope `my`, read `Nil`/`Any` when the call reached the sub
through a punned role. The fix for #18017 (the module's mainline compiled as a
unit mainline, not as a routine body) already covers it; the shape is now part
of `t/modules/import-export/require-in-routine-file-scope-lexical.t`.
