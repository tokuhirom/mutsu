# A scheduler cue's :catch runs at the throw site, and `try .resume` resumes

`$*SCHEDULER.cue(&code, :catch(&handler))` now runs the handler in the dynamic scope of the
`die`, like rakudo, so `.resume` inside it continues the dying code instead of abandoning the
cue. `try` no longer swallows the resume signal, so the common `:catch({ ...; try .resume })`
idiom (and `CATCH { default { try .resume } }`) works. ParaSeq's `.hyperize.grep({ die ... })`
no longer blocks forever waiting for a batch that never completed (#16475).
