# A `--only` re-measure no longer rewrites `index-snapshot.json`, and every container has `bwrap`

Three changes that all belong to the same loop — the `ecosystem-dist-fix` skill, where a mutsu bug
fix is followed by re-measuring the distribution that exposed it.

## The re-measure is now stated as mandatory, and says what may be in the diff

`ecosystem-dist-fix` step 7 already described the `--only` re-measure, but as the last item of the
loop rather than as part of what a fix *is*. It now opens with the rule ("every interpreter change
made in this loop ends with a `--only` re-measure — no exceptions"), says explicitly that it also
applies to the one-method fixes and to a run whose outcome was an issue rather than a patch, and
ends with the check that the ledger side of the diff is nothing but `ecosystem/dists/` records
(`git status --porcelain ecosystem/`). It also asks for a re-measure of the *neighbouring* records
when the root cause plausibly reaches past one distribution — a second record going green for free
is the cheapest evidence available that a fix was general rather than a dressed-up special case.

## `index-snapshot.json` is corpus provenance, so a targeted run leaves it alone

`ecosystem/index-snapshot.json` records which fez/REA index snapshot the ledger was resolved
against. `scripts/ecosystem-sweep.py` rewrote it unconditionally, including on a single-distribution
run — which dated the *whole* corpus to today's index on the strength of one record, and put an
unrelated file into a bug-fix PR's diff where a reviewer reads it as a corpus refresh. It is the
same failure mode `.github/workflows/ecosystem-sweep.yml` already avoids by pinning one index across
all of its shards.

A `--only` run therefore no longer writes the file (it logs `index-snapshot.json: left unchanged`),
and the new `--no-index-snapshot` flag extends that to a `--status` / `--stale` re-measure after a
fix, which has exactly the same problem. Corpus and shard runs still write it — that is what it is
for.

## `bubblewrap` is installed at container setup

The sweep confines every measured interpreter run in `bwrap`, because loading a third-party
distribution already executes its `BEGIN` phasers. Remote containers had no `bwrap`, so the
documentation told agents to fall back to `--sandbox none` for their `--only` re-measure — running
unaudited test-suite code unconfined for want of a two-second `apt-get install`.

`.claude/hooks/session-start.sh` now installs `bubblewrap` alongside rustc, rakudo and the native
libraries, and then runs the same `bwrap --unshare-all --ro-bind / /` probe the CI workflow does:
a container can ship the binary and still deny the unprivileged user namespace it needs, and that
failure would otherwise surface as an unexplained non-zero exit from every measured file. Both the
install and the probe were verified in a remote container (bubblewrap 0.9.0, no AppArmor relaxation
needed), as was a full `--only BTree` sweep under the default sandbox.

So the sandbox is now the default everywhere and `--sandbox none` is reserved for the case where
the hook warned that the probe failed. What still rules a corpus sweep out of a remote container is
its four cores and fixed disk allowance, not the sandbox — dispatch the workflow instead
(`docs/ecosystem-parity.md` §8.1).
