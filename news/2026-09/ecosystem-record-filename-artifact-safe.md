# One colon in a distribution name cost two shards their whole sweep

The first full-corpus sweep ([run 34556792249](https://github.com/tokuhirom/mutsu/actions/runs/34556792249),
`scope: all`) measured shard `A` — 154 distributions, twenty minutes — staged its
records, and then lost every one of them:

```
The path for one of the files in artifact is not valid: /ecosystem/dists/A/App:Racl.json.
Contains the following character:  Colon :
```

`actions/upload-artifact` refuses the characters NTFS cannot hold, and it fails
the **entire artifact** over one path. Shard `S` (109 distributions) went the same
way. Both `Measure` steps had succeeded.

## The rule had a hole in it

`record_path()` mapped `::` to `--`, which is the interesting case and the only
one anybody had thought about. Two fez distributions carry a **single** colon —
`App:Racl` and `Slang:Date` — and a single colon is not `::`, so it went straight
through into the filename. They are one each in shards `A` and `S`.

`ecosystem_common.record_filename()` now owns the mapping: `::` becomes `--`, and
anything left that a filesystem or an artifact upload would reject (`: " < > | *
? % / \`) becomes `%XX`. So `App:Racl` is `App%3ARacl.json`. `%` is escaped too,
which keeps the mapping injective rather than merely safe. The one record already
committed under the old name was renamed in the same change.

## The test existed and ran nowhere

`scripts/ecosystem_common.py --self-test` has covered the TAP parser and the
failure-line extractor since P1. Nothing invoked it — not CI, not the sweep
workflow, not `make test`. The workflow ran `ecosystem-ci.py --self-test` only.

So the filename rule now has cases in that self-test (both real colon names,
every rejected character, and the escape's own `%`), **and the workflow runs both
self-tests** in its `build` job, where they cost about a second before a release
build and 27 rakudo installs.

## And a belt, because the blast radius was absurd

One bad path costing 154 measurements is a cost asymmetry no step should accept,
however careful the escaping is. The staging step now skips a path containing a
rejected character and emits a warning instead of handing it to the upload. With
the escape in place nothing should reach it; if something ever does, it costs one
record rather than a shard.

## Cost

The 25 shards that uploaded still landed — the artifacts and the `collect` job did
their job, and the `--history` guard correctly refused a corpus row for a run
whose shard result was `failure`, which is exactly the case it was written for.
Shards `A` and `S` have to be measured again: their data is gone, because it never
left the runner.
