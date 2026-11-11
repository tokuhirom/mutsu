# Ecosystem sweep now runs twice a day

While ecosystem compatibility is the main focus, `ecosystem-sweep.yml` is scheduled
at 18:20 UTC (03:20 JST) and 06:20 UTC (15:20 JST) instead of once a day, so fixes
show up in the parity numbers within half a day. The runs are serialized by the
existing `concurrency: ecosystem-sweep` group, so they never overlap. Removing the
second `cron` entry restores the nightly cadence.
