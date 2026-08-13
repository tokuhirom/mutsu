# A `Cro::Sink` comes at the end of a pipeline. The `Supply` that returned
# from `sinker` should tap the provided `$pipeline` `Supply`, but should never
# emit any messages itself. The main reason for remaining within the `Supply`
# paradigm is so unhandled errors can be propagated onwards, for reporting.
role Cro::Sink {
    method consumes() { ... }
    method sinker(Supply:D $pipeline) returns Supply:D { ... }
}
