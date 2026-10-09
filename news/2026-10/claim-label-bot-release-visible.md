# The stale-claim auto-release is now visible to the claim log

`.github/scripts/sync-working-label.sh` only reads comments from OWNER, MEMBER and COLLABORATOR
authors, so the `Releasing:` comment that `claim-label.yml` posts as `github-actions[bot]`
(association CONTRIBUTOR) was never seen. The claim stayed live and the scheduled run
"auto-released" it again every three hours (#12026 collected seven of them).

The author filter now also admits a comment from `github-actions[bot]` whose body starts with
`Releasing:`. A release only removes a claim, so this cannot make an issue look taken. The
script's self-test covers the filter.
