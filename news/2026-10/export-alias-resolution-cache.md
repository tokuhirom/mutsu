# Keep export aliases visible to routine resolution

Registering an exported routine now invalidates the name index for every
EXPORT alias it creates. Qualified calls through a module's DEFAULT and ALL
export stashes no longer use a stale index or panic in debug builds. The same
invalidation covers aliases added when a multi routine gains a candidate.
