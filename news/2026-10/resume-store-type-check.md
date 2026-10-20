# A failed typed assignment resumes at the next statement

`{ my Int $x = "a"; say "after"; CATCH { default { .resume } } }` left the
block where rakudo prints `after` (#11691). A resumed exception continued at a
resume point that only call ops recorded. The `X::TypeCheck::Assignment` a
store raised itself therefore had none, and `.resume` ended the protected
region.

The scalar `TypeCheck`/`SetLocal`/`SetLocalDecl` ops now offer their
type-check exception to the resume-capable `CATCH` handlers at the raise site,
as `die` does (ADR-0072). A resumed store leaves its container unchanged: a
declaration keeps its type object, an assignment its old value, as in rakudo.
`X::Assignment::RO` stays non-resumable, also as in rakudo.
