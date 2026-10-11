# The ecosystem roulette becomes an ordered queue

Agents no longer draw a random failing distribution out of the ecosystem ledger. With only ~240
actionable records left (`red`, `partial`, `blocked_load` out of 1641 measured), the random draw
had done its job: it existed to stop agents cherry-picking cheap records and gaming the parity
KPI, and working the whole remainder in a fixed order rules that out just as well.

`scripts/ecosystem-tickets.py --queue` prints that order. A distribution ranks higher the more
other measured distributions have it in their dependency closure, then the fewer failing baseline
files it has; confirmed dead ends (`ecosystem/exclude.txt`), `blocked_load` records rakudo cannot
load either, and distributions whose every failing file is an accepted divergence are left out.
Each row carries its first failure's root-cause signature and, when that signature fails three or
more distributions, the cluster id to file it under. On the 2026-10-10 ledger the head is
JSON::Fast (545 dependents), META6 (267), Log::Timeline (91), LibraryMake (60), Method::Also (53)
and App::Prove6 (49).

The `ecosystem-dist-roulette` skill is now `ecosystem-dist-queue`: take the highest-ranked
distribution that is neither locked on the lock board nor already tracked by an open issue, lock
it, diagnose it, and then route it by where its root cause lives — fix a small self-contained
cause directly, file a cause in a campaign's area (env, method-table rows, RakuAST) or a busy lane
as a lane-labelled issue, and file a signature shared by three or more distributions once as a
cluster issue. When every remaining entry waits on an issue or a campaign, the run stops; the
queue is re-checked after each nightly sweep. The lock protocol is unchanged, and `pick-dist.py`
is gone.
