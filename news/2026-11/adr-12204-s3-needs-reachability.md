# ADR-12204: S3 needs a reachability answer, not more capture sources

Measuring S3 after S2's spawn-site capture work showed that retiring
`container_name_is_redeclared` still fails the same three thread-lock files: they
involve aggregates the spawned block does not reference but a callee might reach,
which the lane still seeds. The ADR now records that S3 depends on a call-graph
free-variable closure (or cells at routine closure creation), and that the
hyper/race batch threads need a loop-body free-variable pass first.
