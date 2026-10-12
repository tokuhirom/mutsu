# Native `&code` parameters reject non-Callables

`Promise.start({ })` returned a Promise, although `{ }` in argument position is an empty
Hash and rakudo fails the bind of its `&code` parameter. The natively implemented
`&`-parameters now bind against `Callable` through one helper,
`Interpreter::bind_callable_param`, and raise rakudo's
`X::TypeCheck::Binding::Parameter` ("expected Callable but got Hash ({})"):
`Promise.start`, `.then`, `.andthen`, `.orelse` (`&code`) and `Supply.on-demand`
(`&producer`). `$*SCHEDULER.cue` is a multi in rakudo, so a non-Callable there is
`X::Multi::NoMatch`. (#12801)
