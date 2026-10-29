# `nqp::throw` of a warning with a user payload reaches `CONTROL { when TheClass }`

`nqp::throw` of a `CONTROL_WARN` exception whose payload is a user `CX::Warn` subclass object
(Red's `X::Red::Transaction::AsyncResult.warn`) dropped the payload and warned with the bare
message, so `CONTROL { when TheClass { ...; .resume } }` never ran. It is now thrown as that
object: the handler gets it as its topic and `.resume` continues after the throw; when no handler
takes it, the default warning handler prints it and the throw returns. Red `t/99-transaction-guard`
"do does not extend its participation for a returned Promise" depends on it.
