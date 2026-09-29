# Ecosystem lock board rotated again: #8977 -> #10045

The `ecosystem:lock` board on [#8977](https://github.com/tokuhirom/mutsu/issues/8977) reached 301
comments in one week, after the #7884 -> #8977 rotation. At that size a single `get_comments` call
can no longer read the whole log, and agents read it that way before they lock a distribution.

The board rotated to [#10045](https://github.com/tokuhirom/mutsu/issues/10045). Only one lock was
still live on #8977, `Pod::To::HTML`, and it was carried forward. `ASN::BER` was not carried. Its
holder had released it with a malformed `Releasing:` line, and its branch is gone from origin.

Six locks were first carried by mistake, from a stale reading of the log. The mistake was fixed
on the board itself, by appending `Unlocking:` lines to #10045 and a correction on #8977. No
comment was edited.

The `ecosystem:lock` label moved to #10045, and #8977 was closed with a redirect. These files now
point at #10045:

- the `ecosystem-dist-roulette` and `ecosystem-dist-fix` skills
- `ecosystem/README.md`
- `docs/ecosystem-parity.md`
- `docs/issue-workflow.md`
- `PLAN.md`
- `.github/scripts/sync-working-label.sh`

Past `news/` entries keep the old number, because they record history. The new board lists the
files to update in its Rotation section, and says to rotate at about 300 comments.
