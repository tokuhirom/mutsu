# Gauge is left out of the ecosystem work queue

`Gauge` dies at load on `$*VM.version < v2022.04`, the MoarVM release coordinate. mutsu is not a
MoarVM and will not report one, and the module is not handled until it supports mutsu (user decision
2026-10-29, #15266). It now has an `ecosystem/exclude.txt` entry, so the dist queue and the bulk
sweeps stop offering it; `ecosystem-sweep.py --only Gauge` still measures it for a re-check.
