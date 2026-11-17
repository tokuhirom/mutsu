# Plain Supplier releases delivered history

A plain `Supplier` now discards emitted values once no pending react setup
needs to replay them. It still tracks the total emission count and final value,
so interval ticks and completion promises retain their existing behavior.
`Supplier::Preserving` continues to buffer values for its next tap.
