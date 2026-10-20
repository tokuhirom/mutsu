# A sunk routine call runs its result's `sink`

`sub g { $x }; g;` ran no user `sink` method of `$x`'s object, though rakudo
does: a routine that is not `is rw`/`is raw` decontainerizes its implicit
result, and the statement sinks that value (#11537). mutsu's call result
carries no container, so every function call was compiled not to user-sink.

A statement call now compiles to `SinkPopCall`. Once the value in hand has a
user `sink`, it asks the callee's declaration whether the routine hands back a
decontainerized value: it is not `is rw`/`is raw`/`return-rw`, it has no
explicit `return`, and its tail is not a readonly `$` parameter. That fact,
`RoutineBodyFacts::result_is_decontainerized`, is computed once per routine.
The rule follows rakudo's own measured behavior. It stays conservative where
rakudo differs case by case (an explicit `return S.new` is not sunk).
