# The ledger's filename rule was not injective, and one bad tarball killed a shard

Three more ways the first full-corpus sweep lost data, all found by reading its
27 shard logs rather than its exit status.

## `::` → `--` is not injective, and the corpus proves it

The record filename mapped `String::Utils` to `String--Utils.json`, on the
assumption that no distribution name contains a literal `--`. Two do, and each has
a `::` twin:

| distinct distributions | filename they shared |
|---|---|
| `Qwiratry::Location::HTTP`, `Qwiratry--Location--HTTP` | `Qwiratry--Location--HTTP.json` |
| `WWW::CloudHosting::Hetzner`, `WWW--CloudHosting--Hetzner` | `WWW--CloudHosting--Hetzner.json` |

So the ledger silently held one measurement for two distributions, with whichever
shard finished last deciding the winner — and `--only` on the loser would read the
other's record, including its provenance, which is how a stale record can look
fresh.

## A case-insensitive artifact upload drops the other kind of twin

```
Uploads are case insensitive: /tmp/eco-staging/ecosystem/dists/C/CSV-Autoclass.json
was detected that it will be overwritten by another file with the same path
```

`CSV-AutoClass` and `CSV-Autoclass` are both real, and so are `Config::INI` and
`Config::Ini`. Their filenames differ only by case, which git on Linux is happy
with — but an artifact upload is case-insensitive, so one of each pair never made
it out of the shard. The same two pairs would break a checkout on macOS or
Windows.

Both problems are one problem: the filename was not a faithful key. It is now
`<stem>~<8 hex of sha256(name)>.json`, so `String::Utils` is
`String--Utils~aed281d9.json`. The stem stays readable and legal (`::` → `--`,
artifact-hostile characters percent-escaped, from the fix earlier that day); the
digest makes the mapping injective including case-insensitively, and it cannot rot
as the index grows. A disambiguate-only-when-needed rule would have been prettier
and would rename an existing record the day a colliding distribution is published.
All 1423 records in the ledger were renamed in the same change, and
`ecosystem_common`'s self-test now pins all four real pairs. The rollup over the
renamed tree reproduces the pre-rename `ecosystem/summary.json` byte for byte,
which is the check that a rename moved no measurement.

## One unpackable tarball killed the whole shard

Shard `C` reported success, uploaded 92 records, and had in fact died at
distribution 93 of 124:

```
tarfile.AbsoluteLinkError: 'dist/trial/OgdenWebb/plugins' is a link to an absolute path
```

Python's `filter="data"` extraction refuses an absolute symlink, correctly — one
of 1624 unaudited archives contains one. The exception went straight out of
`sweep_dist`, through `pool.map`, and terminated the sweep process, so the 32
distributions after it were never measured. The 92 before it survived only because
the staging and upload steps run under `if: always()`, which had been added that
morning for exactly this shape of accident.

`work()` now catches per distribution and records a `skipped` entry whose `note`
carries the exception. Nothing was measured for it, so charging it to mutsu would
be a lie; being absent from the ledger would be worse, because absence reads as
"not yet swept".

## Two guards, so neither mistake can be made quietly again

Renaming 1423 records is a one-line change away from doubling the ledger: leave an
old-named file behind and the corpus grows by one distribution whose numbers are
counted twice. `load_records()` now refuses to load a ledger in which two files
claim the same `dist`, naming both paths. It is the rename's own safety net, and
it costs one dictionary.

The other guard is about what a sweep may publish. `ecosystem/` carries no
summary until a full corpus sweep has produced one, and the workflow tested that
by asking whether `scope` was `all` -- which is a request, not a result. Run 4
asked for all 27 shards, lost two of them, and created the first
`ecosystem/summary.json` over 1423 of 1624 distributions. Creating the first
summary now needs what `--history` needs: `scope=all` *and* every shard green.

## What the pattern says

Each of these was invisible in the run's status: shard `C` was green in the job
list while a third of it was missing, and the two filename collapses produce a
ledger that is merely *wrong* rather than obviously broken. The sweep's own logs
are the only place they showed up, and only the `if: always()` belt kept the
accident from being total. The lesson recorded here is to read a corpus run's logs
even when its conclusion is success — and, where a step can lose an hour of
measurement, to make the loss cost one record instead.
