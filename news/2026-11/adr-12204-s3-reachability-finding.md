# ADR-12204 S3: a reachability filter alone is unsound

Measured an op-walk reachability filter for spawn seeding: it makes the three known-red lane tests green but breaks a supply tap callback whose aggregate is not yet celled, because every spawn implicitly publishes every aggregate for later callbacks. The ADR now orders the remaining work: all callback capture sources and declaration-time cells first, the filter last.
