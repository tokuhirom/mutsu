# An `Exception` subclass without a `message` answers the `X::StubCode` Failure

rakudo declares `Exception.message` as a stub (`method message() { ... }`), so a
class that is an `Exception` but never implemented `message` answers a `Failure`
wrapping `X::StubCode`, which throws as soon as it is used as text. mutsu answered
the empty string, for an `X::`-named class and for any other. The built-in
`Exception`, `X::AdHoc` and `X::OS` keep their previous answer (rakudo's bare
`Exception.new.message` is a Failure too; that is left as a TODO).
LLM::Data::Inference's `t/15-messageless-exceptions.rakutest` passes.
